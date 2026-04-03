import Time "mo:core/Time";
import Char "mo:base/Char";
import Array "mo:base/Array";
import AccessControl "authorization/access-control";
import MixinAuthorization "authorization/MixinAuthorization";
import Principal "mo:core/Principal";

actor {

  type Role = { #admin; #staff; #supplier };

  type User = {
    id          : Text;
    username    : Text;
    password    : Text;
    role        : Role;
    businessIds : [Text];
    createdAt   : Int;
  };

  type Business = { id : Text; name : Text };
  type Godown   = { id : Text; name : Text; businessId : Text };

  type SubCategory = {
    id        : Text;
    name      : Text;
    fieldType : Text;
    options   : [Text];
  };

  // Category type kept WITHOUT businessId to preserve stable variable compatibility.
  // Business mapping is tracked via categoryBusinessMap below.
  type Category = {
    id            : Text;
    name          : Text;
    subCategories : [SubCategory];
  };

  // CategoryV2 — stores businessId directly on the record.
  // This is the authoritative type used by all public functions.
  // categoriesV2 is the source of truth; `categories` is kept only for stable variable upgrade compatibility.
  type CategoryV2 = {
    id            : Text;
    name          : Text;
    subCategories : [SubCategory];
    businessId    : Text;
  };

  type BiltyPrefix     = { id : Text; prefix : Text };
  type TransportTracker = { id : Text; transport : Text; trackingUrl : Text };
  type LoginResult      = { #ok : User; #err : Text };

  type TransitEntry = {
    id          : Text;
    biltyNumber : Text;
    transport   : Text;
    supplier    : Text;
    category    : Text;
    itemName    : Text;
    packages    : Int;
    biltyDate   : Text;
    businessId  : Text;
    enteredBy   : Text;
    createdAt   : Int;
  };

  type QueueBale  = { baleLabel : Text; category : Text; itemName : Text; status : Text };

  type QueueEntry = {
    id          : Text;
    biltyNumber : Text;
    transport   : Text;
    supplier    : Text;
    bales       : [QueueBale];
    businessId  : Text;
    enteredBy   : Text;
    createdAt   : Int;
    delivered   : Bool;
  };

  type GodownQty = { godownId : Text; qty : Int };

  type InwardItem = {
    category     : Text;
    itemName     : Text;
    subCategory  : Text;
    totalQty     : Int;
    shopQty      : Int;
    godownQtys   : [GodownQty];
    purchaseRate : Float;
    saleRate     : Float;
  };

  type InwardSavedEntry = {
    id          : Text;
    biltyNumber : Text;
    transport   : Text;
    supplier    : Text;
    savedBy     : Text;
    savedAt     : Int;
    businessId  : Text;
    items       : [InwardItem];
  };

  type InventoryItem = {
    id           : Text;
    businessId   : Text;
    category     : Text;
    itemName     : Text;
    subCategory  : Text;
    godownQtys   : [GodownQty];
    shopQty      : Int;
    purchaseRate : Float;
    saleRate     : Float;
  };

  type TransferEntry = {
    id            : Text;
    businessId    : Text;
    category      : Text;
    itemName      : Text;
    subCategory   : Text;
    fromType      : Text;
    fromId        : Text;
    toType        : Text;
    toId          : Text;
    qty           : Int;
    rate          : Float;
    transferredBy : Text;
    createdAt     : Int;
  };

  type DeliveryLineItem = {
    category    : Text;
    itemName    : Text;
    subCategory : Text;
    qty         : Int;
    godownId    : Text;
  };

  type DeliveryEntry = {
    id            : Text;
    businessId    : Text;
    deliveryType  : Text;
    biltyNumber   : Text;
    customerName  : Text;
    customerPhone : Text;
    items         : [DeliveryLineItem];
    deliveredBy   : Text;
    createdAt     : Int;
  };

  type SaleLineItem = {
    category    : Text;
    itemName    : Text;
    subCategory : Text;
    qty         : Int;
    rate        : Float;
  };

  type SaleEntry = {
    id         : Text;
    businessId : Text;
    items      : [SaleLineItem];
    recordedBy : Text;
    createdAt  : Int;
  };

  type TxType = { #inward; #transfer; #delivery; #sale; #directStock };

  type TxRecord = {
    id           : Text;
    businessId   : Text;
    txType       : TxType;
    biltyNumber  : Text;
    category     : Text;
    itemName     : Text;
    subCategory  : Text;
    fromLocation : Text;
    toLocation   : Text;
    transport    : Text;
    qty          : Int;
    rate         : Float;
    enteredBy    : Text;
    notes        : Text;
    createdAt    : Int;
  };

  stable var users             : [User]             = [];
  stable var businesses        : [Business]         = [];
  stable var godowns           : [Godown]           = [];
  stable var categories        : [Category]         = [];
  stable var categoriesV2      : [CategoryV2]       = [];
  stable var biltyPrefixes     : [BiltyPrefix]      = [];
  stable var transportTrackers : [TransportTracker] = [];
  stable var transitEntries    : [TransitEntry]     = [];
  stable var queueEntries      : [QueueEntry]       = [];
  stable var inwardSaved       : [InwardSavedEntry] = [];
  stable var inventory         : [InventoryItem]    = [];
  stable var transfers         : [TransferEntry]    = [];
  stable var deliveries        : [DeliveryEntry]    = [];
  stable var sales             : [SaleEntry]        = [];
  stable var txHistory         : [TxRecord]         = [];
  stable var appSettings       : Text               = "{}";
  // (categoryId, businessId) — kept as stable var for backward compatibility.
  // Populated reliably by addCategory, seed, and the repair step in ensureCategoryMap.
  stable var categoryBusinessMap : [(Text, Text)]   = [];

  let accessControlState = AccessControl.initState();
  include MixinAuthorization(accessControlState);

  stable var seeded      : Bool = false;
  stable var seedVersion : Nat  = 0;

  // ---- Map helpers ----

  // Return the businessId for a category from the map, or "" if not found.
  func getCatBusiness(catId : Text) : Text {
    let found = Array.find(categoryBusinessMap, func((cId, _bId) : (Text, Text)) : Bool { cId == catId });
    switch (found) {
      case (?(_, bId)) bId;
      case null "";
    }
  };

  // Ensure every category in `categories` has an entry in categoryBusinessMap.
  // Derives business from ID prefix (b1-*, b2-*, etc.) before falling back to "b1".
  func inferBusinessFromId(catId : Text) : Text {
    if (catId.size() > 3) {
      let prefix = catId.chars();
      var buf = "";
      var dashCount = 0;
      label scan for (ch in prefix) {
        if (ch == '-') {
          dashCount += 1;
          break scan;
        };
        buf := buf # (Char.toText(ch));
      };
      if (dashCount > 0 and buf != "") {
        let match = Array.find(businesses, func(b : Business) : Bool { b.id == buf });
        switch (match) {
          case (?b) return b.id;
          case null {};
        };
      };
    };
    "b1"
  };

  func ensureCategoryMap() {
    for (c in categories.vals()) {
      let inMap = Array.find(categoryBusinessMap, func((cId, _) : (Text, Text)) : Bool { cId == c.id });
      switch (inMap) {
        case null {
          let bizId = inferBusinessFromId(c.id);
          categoryBusinessMap := Array.append(categoryBusinessMap, [(c.id, bizId)]);
        };
        case (?_) {};
      };
    };
  };

  // ---- Seed ----

  func seed() {
    if (seedVersion >= 4) {
      // Versions 5 and 6 previously called ensureCategoryMap() which could corrupt
      // the map by incorrectly inferring business ownership from category IDs.
      // Now we just bump the version without any map repairs.
      if (seedVersion < 5) { seedVersion := 5 };
      if (seedVersion < 6) { seedVersion := 6 };
      // Version 7: no-op bump to skip any future bad repair calls
      if (seedVersion < 7) { seedVersion := 7 };
      // Version 8: migrate legacy categories → categoriesV2 with direct businessId field
      if (seedVersion < 8) {
        seedVersion := 8;
        if (categoriesV2.size() == 0 and categories.size() > 0) {
          var v2 : [CategoryV2] = [];
          for (c in categories.vals()) {
            // Find all businesses this category belongs to via the map
            let bizMappings = Array.filter(categoryBusinessMap, func((cId, _) : (Text, Text)) : Bool { cId == c.id });
            if (bizMappings.size() == 0) {
              // Unmapped legacy → belongs to b1
              v2 := Array.append(v2, [{ id = c.id; name = c.name; subCategories = c.subCategories; businessId = "b1" }]);
            } else {
              for ((_, bId) in bizMappings.vals()) {
                // If shared across multiple businesses, create unique IDs per business
                let uniqueId = if (bizMappings.size() > 1) { bId # "-" # c.id } else { c.id };
                v2 := Array.append(v2, [{ id = uniqueId; name = c.name; subCategories = c.subCategories; businessId = bId }]);
              };
            };
          };
          categoriesV2 := v2;
        };
      };
      return;
    };
    seedVersion := 4;
    businesses := [{ id = "b1"; name = "Demo Business" }];
    godowns    := [
      { id = "g1"; name = "Main Godown";   businessId = "b1" },
      { id = "g2"; name = "Second Godown"; businessId = "b1" }
    ];
    categories := [
      { id = "cat1"; name = "Safi";
        subCategories = [
          { id = "sc1"; name = "Size";  fieldType = "text";   options = [] },
          { id = "sc2"; name = "Color"; fieldType = "select"; options = ["black","tiranga","mix"] }
        ]
      },
      { id = "cat2"; name = "Lungi";
        subCategories = [
          { id = "sc3"; name = "Size";  fieldType = "select"; options = ["2 mtr","2.25 mtr","2.5 mtr"] },
          { id = "sc4"; name = "Color"; fieldType = "select"; options = ["plain white","plain colour","mix"] }
        ]
      },
      { id = "cat3"; name = "Napkin";
        subCategories = [
          { id = "sc5"; name = "Size"; fieldType = "select"; options = ["14x21","12x18","16x24"] }
        ]
      }
    ];
    // Seed the category map for all seeded categories under b1
    categoryBusinessMap := [("cat1","b1"),("cat2","b1"),("cat3","b1")];
    // Seed categoriesV2 directly (source of truth)
    categoriesV2 := [
      { id = "cat1"; name = "Safi";   businessId = "b1";
        subCategories = [
          { id = "sc1"; name = "Size";  fieldType = "text";   options = [] },
          { id = "sc2"; name = "Color"; fieldType = "select"; options = ["black","tiranga","mix"] }
        ]
      },
      { id = "cat2"; name = "Lungi";  businessId = "b1";
        subCategories = [
          { id = "sc3"; name = "Size";  fieldType = "select"; options = ["2 mtr","2.25 mtr","2.5 mtr"] },
          { id = "sc4"; name = "Color"; fieldType = "select"; options = ["plain white","plain colour","mix"] }
        ]
      },
      { id = "cat3"; name = "Napkin"; businessId = "b1";
        subCategories = [
          { id = "sc5"; name = "Size"; fieldType = "select"; options = ["14x21","12x18","16x24"] }
        ]
      }
    ];
    biltyPrefixes := [
      { id = "p1"; prefix = "sola" },
      { id = "p2"; prefix = "erob" },
      { id = "p3"; prefix = "cheb" },
      { id = "p4"; prefix = "0"    }
    ];
    users := [
      { id = "u1"; username = "admin";     password = "password"; role = #admin;    businessIds = ["b1"]; createdAt = 0 },
      { id = "u2"; username = "staff";     password = "password"; role = #staff;    businessIds = ["b1"]; createdAt = 0 },
      { id = "u3"; username = "supplier";  password = "password"; role = #supplier; businessIds = ["b1"]; createdAt = 0 }
    ];
    transportTrackers := [];
  };

  public func login(username : Text, password : Text) : async LoginResult {
    seed();
    let match = Array.find(users, func(u : User) : Bool {
      u.username == username and u.password == password
    });
    switch (match) {
      case (?u) #ok(u);
      case null  #err("Invalid username or password");
    };
  };

  public query func getUsers() : async [User] { users };

  public func addUser(id : Text, username : Text, password : Text, role : Role, businessIds : [Text]) : async () {
    seed();
    users := Array.append(users, [{ id; username; password; role; businessIds; createdAt = Time.now() }]);
  };

  public func updateUser(id : Text, username : Text, password : Text, role : Role, businessIds : [Text]) : async () {
    users := Array.map(users, func(u : User) : User {
      if (u.id == id) { { id; username; password; role; businessIds; createdAt = u.createdAt } } else u
    });
  };

  public func deleteUser(id : Text) : async () {
    users := Array.filter(users, func(u : User) : Bool { u.id != id });
  };

  public query func getBusinesses() : async [Business] { businesses };

  public func addBusiness(id : Text, name : Text) : async () {
    seed(); businesses := Array.append(businesses, [{ id; name }]);
  };

  public func updateBusiness(id : Text, name : Text) : async () {
    businesses := Array.map(businesses, func(b : Business) : Business {
      if (b.id == id) { { id; name } } else b
    });
  };

  public func deleteBusiness(id : Text) : async () {
    businesses := Array.filter(businesses, func(b : Business) : Bool { b.id != id });
  };

  public query func getGodowns() : async [Godown] { godowns };

  public query func getGodownsByBusiness(businessId : Text) : async [Godown] {
    Array.filter(godowns, func(g : Godown) : Bool {
      g.businessId == businessId or (g.businessId == "" and businessId == "b1")
    })
  };

  public func addGodown(id : Text, name : Text, businessId : Text) : async () {
    seed(); godowns := Array.append(godowns, [{ id; name; businessId }]);
  };

  public func updateGodown(id : Text, name : Text, businessId : Text) : async () {
    godowns := Array.map(godowns, func(g : Godown) : Godown {
      if (g.id == id) { { id; name; businessId } } else g
    });
  };

  public func deleteGodown(id : Text) : async () {
    godowns := Array.filter(godowns, func(g : Godown) : Bool { g.id != id });
  };

  public query func getCategories() : async [CategoryV2] { categoriesV2 };

  // Filter by the map; unmapped categories default to b1 (legacy compatibility).
  // Check if this category has any map entry for the given businessId.
  // Supports both isolated categories (business-prefixed IDs) and shared categories
  // (same ID in map multiple times for different businesses).
  func catBelongsToBusiness(catId : Text, businessId : Text) : Bool {
    let directMatch = Array.find(categoryBusinessMap, func((cId, bId) : (Text, Text)) : Bool {
      cId == catId and bId == businessId
    });
    switch (directMatch) {
      case (?_) true;
      case null {
        // No entry for this (catId, businessId) pair.
        // Check if catId has ANY mapping at all — if not, default to b1 (legacy).
        let anyMapping = Array.find(categoryBusinessMap, func((cId, _) : (Text, Text)) : Bool { cId == catId });
        switch (anyMapping) {
          case null { businessId == "b1" }; // truly unmapped legacy category → b1
          case (?_) false;                  // mapped to other business(es), not this one
        }
      };
    }
  };

  public query func getCategoriesByBusiness(businessId : Text) : async [CategoryV2] {
    Array.filter(categoriesV2, func(c : CategoryV2) : Bool {
      c.businessId == businessId or (c.businessId == "" and businessId == "b1")
    })
  };

  public func addCategory(id : Text, name : Text, businessId : Text) : async () {
    seed();
    // Prevent duplicate: only add if this exact (id, businessId) pair doesn't exist
    let exists = Array.find(categoriesV2, func(c : CategoryV2) : Bool {
      c.id == id and c.businessId == businessId
    });
    switch (exists) {
      case null {
        categoriesV2 := Array.append(categoriesV2, [{ id; name; subCategories = []; businessId }]);
        // Also update legacy map for backward compat
        categoryBusinessMap := Array.append(categoryBusinessMap, [(id, businessId)]);
      };
      case (?_) {}; // Already exists for this business — no-op
    };
  };

  public func updateCategory(id : Text, name : Text) : async () {
    categoriesV2 := Array.map(categoriesV2, func(c : CategoryV2) : CategoryV2 {
      if (c.id == id) { { id; name; subCategories = c.subCategories; businessId = c.businessId } } else c
    });
  };

  // Business-scoped delete: removes only this business's category entry from categoriesV2.
  public func deleteCategory(id : Text, businessId : Text) : async () {
    categoriesV2 := Array.filter(categoriesV2, func(c : CategoryV2) : Bool {
      not (c.id == id and c.businessId == businessId)
    });
    categoryBusinessMap := Array.filter(categoryBusinessMap, func((cId, bId) : (Text, Text)) : Bool {
      not (cId == id and bId == businessId)
    });
  };

  // Admin-level global delete (used in restore flow only)
  public func deleteCategoryGlobal(id : Text) : async () {
    categoriesV2 := Array.filter(categoriesV2, func(c : CategoryV2) : Bool { c.id != id });
    categories := Array.filter(categories, func(c : Category) : Bool { c.id != id });
    categoryBusinessMap := Array.filter(categoryBusinessMap, func((cId,_) : (Text,Text)) : Bool { cId != id });
  };

  public func addSubCategory(categoryId : Text, sc : SubCategory) : async () {
    categoriesV2 := Array.map(categoriesV2, func(c : CategoryV2) : CategoryV2 {
      if (c.id == categoryId) {
        { id = c.id; name = c.name; subCategories = Array.append(c.subCategories, [sc]); businessId = c.businessId }
      } else c
    });
  };

  public func updateSubCategory(categoryId : Text, sc : SubCategory) : async () {
    categoriesV2 := Array.map(categoriesV2, func(c : CategoryV2) : CategoryV2 {
      if (c.id == categoryId) {
        { id = c.id; name = c.name; businessId = c.businessId;
          subCategories = Array.map(c.subCategories, func(s : SubCategory) : SubCategory {
            if (s.id == sc.id) sc else s
          })
        }
      } else c
    });
  };

  public func deleteSubCategory(categoryId : Text, subCategoryId : Text) : async () {
    categoriesV2 := Array.map(categoriesV2, func(c : CategoryV2) : CategoryV2 {
      if (c.id == categoryId) {
        { id = c.id; name = c.name; businessId = c.businessId;
          subCategories = Array.filter(c.subCategories, func(s : SubCategory) : Bool { s.id != subCategoryId })
        }
      } else c
    });
  };

  public query func getBiltyPrefixes() : async [BiltyPrefix] { biltyPrefixes };

  public func addBiltyPrefix(id : Text, prefix : Text) : async () {
    seed(); biltyPrefixes := Array.append(biltyPrefixes, [{ id; prefix }]);
  };

  public func deleteBiltyPrefix(id : Text) : async () {
    biltyPrefixes := Array.filter(biltyPrefixes, func(p : BiltyPrefix) : Bool { p.id != id });
  };

  public query func getTransportTrackers() : async [TransportTracker] { transportTrackers };

  public func addTransportTracker(id : Text, transport : Text, trackingUrl : Text) : async () {
    seed(); transportTrackers := Array.append(transportTrackers, [{ id; transport; trackingUrl }]);
  };

  public func updateTransportTracker(id : Text, transport : Text, trackingUrl : Text) : async () {
    transportTrackers := Array.map(transportTrackers, func(t : TransportTracker) : TransportTracker {
      if (t.id == id) { { id; transport; trackingUrl } } else t
    });
  };

  public func deleteTransportTracker(id : Text) : async () {
    transportTrackers := Array.filter(transportTrackers, func(t : TransportTracker) : Bool { t.id != id });
  };

  public query func getTransitEntries(businessId : Text) : async [TransitEntry] {
    Array.filter(transitEntries, func(e : TransitEntry) : Bool { e.businessId == businessId })
  };

  public func addTransitEntry(entry : TransitEntry) : async () {
    seed(); transitEntries := Array.append(transitEntries, [entry]);
  };

  public func updateTransitEntry(entry : TransitEntry) : async () {
    transitEntries := Array.map(transitEntries, func(e : TransitEntry) : TransitEntry {
      if (e.id == entry.id) entry else e
    });
  };

  public func deleteTransitEntry(id : Text) : async () {
    transitEntries := Array.filter(transitEntries, func(e : TransitEntry) : Bool { e.id != id });
  };

  public func biltyExists(biltyNumber : Text) : async Bool {
    let inTransit = Array.find(transitEntries, func(e : TransitEntry) : Bool { e.biltyNumber == biltyNumber });
    let inQueue   = Array.find(queueEntries,   func(e : QueueEntry)   : Bool { e.biltyNumber == biltyNumber });
    let inSaved   = Array.find(inwardSaved,    func(e : InwardSavedEntry) : Bool { e.biltyNumber == biltyNumber });
    switch (inTransit) { case (?_) return true; case null {} };
    switch (inQueue)   { case (?_) return true; case null {} };
    switch (inSaved)   { case (?_) return true; case null {} };
    false
  };

  public query func getQueueEntries(businessId : Text) : async [QueueEntry] {
    Array.filter(queueEntries, func(e : QueueEntry) : Bool { e.businessId == businessId and not e.delivered })
  };

  public func addQueueEntry(entry : QueueEntry) : async () {
    seed();
    queueEntries   := Array.append(queueEntries, [entry]);
    transitEntries := Array.filter(transitEntries, func(e : TransitEntry) : Bool { e.biltyNumber != entry.biltyNumber });
  };

  public func updateQueueEntry(entry : QueueEntry) : async () {
    queueEntries := Array.map(queueEntries, func(e : QueueEntry) : QueueEntry {
      if (e.id == entry.id) entry else e
    });
  };

  public func markQueueDelivered(id : Text) : async () {
    queueEntries := Array.map(queueEntries, func(e : QueueEntry) : QueueEntry {
      if (e.id == id) {
        { id = e.id; biltyNumber = e.biltyNumber; transport = e.transport;
          supplier = e.supplier; bales = e.bales; businessId = e.businessId;
          enteredBy = e.enteredBy; createdAt = e.createdAt; delivered = true }
      } else e
    });
  };

  public func deleteQueueEntry(id : Text) : async () {
    queueEntries := Array.filter(queueEntries, func(e : QueueEntry) : Bool { e.id != id });
  };

  public query func getInwardSaved(businessId : Text) : async [InwardSavedEntry] {
    Array.filter(inwardSaved, func(e : InwardSavedEntry) : Bool { e.businessId == businessId })
  };

  public func saveInward(entry : InwardSavedEntry) : async () {
    seed();
    let exists = Array.find(inwardSaved, func(e : InwardSavedEntry) : Bool { e.id == entry.id });
    switch (exists) {
      case (?_) { /* already saved */ };
      case null { inwardSaved := Array.append(inwardSaved, [entry]) };
    };
    transitEntries := Array.filter(transitEntries, func(e : TransitEntry) : Bool { e.biltyNumber != entry.biltyNumber });
    queueEntries   := Array.filter(queueEntries,   func(e : QueueEntry)   : Bool { e.biltyNumber != entry.biltyNumber });
  };

  public func updateInwardSaved(entry : InwardSavedEntry) : async () {
    inwardSaved := Array.map(inwardSaved, func(e : InwardSavedEntry) : InwardSavedEntry {
      if (e.id == entry.id) entry else e
    });
  };

  public func deleteInwardSaved(id : Text) : async () {
    inwardSaved := Array.filter(inwardSaved, func(e : InwardSavedEntry) : Bool { e.id != id });
  };

  public query func getInventory(businessId : Text) : async [InventoryItem] {
    Array.filter(inventory, func(i : InventoryItem) : Bool {
      i.businessId == businessId or (i.businessId == "" and businessId == "b1")
    })
  };

  public func addInventoryItem(item : InventoryItem) : async () {
    seed();
    let exists = Array.find(inventory, func(i : InventoryItem) : Bool { i.id == item.id });
    switch (exists) {
      case (?_) {
        inventory := Array.map(inventory, func(i : InventoryItem) : InventoryItem {
          if (i.id == item.id) item else i
        });
      };
      case null { inventory := Array.append(inventory, [item]) };
    };
  };

  public func updateInventoryItem(item : InventoryItem) : async () {
    inventory := Array.map(inventory, func(i : InventoryItem) : InventoryItem {
      if (i.id == item.id) item else i
    });
  };

  public func deleteInventoryItem(id : Text) : async () {
    inventory := Array.filter(inventory, func(i : InventoryItem) : Bool { i.id != id });
  };

  func applyInventoryAddition(businessId : Text, item : InwardItem) {
    let key = businessId # "|" # item.category # "|" # item.itemName # "|" # item.subCategory;
    let existing = Array.find(inventory, func(i : InventoryItem) : Bool {
      i.businessId == businessId and i.category == item.category
        and i.itemName == item.itemName and i.subCategory == item.subCategory
    });
    switch (existing) {
      case null {
        inventory := Array.append(inventory, [{
          id = key; businessId; category = item.category; itemName = item.itemName;
          subCategory = item.subCategory; godownQtys = item.godownQtys;
          shopQty = item.shopQty; purchaseRate = item.purchaseRate; saleRate = item.saleRate;
        }]);
      };
      case (?inv) {
        var merged = inv.godownQtys;
        for (gq in item.godownQtys.vals()) {
          let found = Array.find(merged, func(g : GodownQty) : Bool { g.godownId == gq.godownId });
          switch (found) {
            case null { merged := Array.append(merged, [gq]) };
            case (?_) {
              merged := Array.map(merged, func(g : GodownQty) : GodownQty {
                if (g.godownId == gq.godownId) { { godownId = g.godownId; qty = g.qty + gq.qty } }
                else g
              });
            };
          };
        };
        inventory := Array.map(inventory, func(i : InventoryItem) : InventoryItem {
          if (i.id == inv.id) {
            { id = inv.id; businessId = inv.businessId; category = inv.category;
              itemName = inv.itemName; subCategory = inv.subCategory; godownQtys = merged;
              shopQty = inv.shopQty + item.shopQty;
              purchaseRate = item.purchaseRate; saleRate = item.saleRate; }
          } else i
        });
      };
    };
  };

  public query func getTransfers(businessId : Text) : async [TransferEntry] {
    Array.filter(transfers, func(t : TransferEntry) : Bool { t.businessId == businessId })
  };

  public func postTransfer(entry : TransferEntry) : async Text {
    seed();
    let inv = Array.find(inventory, func(i : InventoryItem) : Bool {
      i.businessId == entry.businessId and i.category == entry.category
        and i.itemName == entry.itemName and i.subCategory == entry.subCategory
    });
    switch (inv) {
      case null return "Item not found in inventory";
      case (?item) {
        if (entry.fromType == "godown") {
          let gq = Array.find(item.godownQtys, func(g : GodownQty) : Bool { g.godownId == entry.fromId });
          switch (gq) {
            case null  return "Godown not found";
            case (?g) {
              if (g.qty < entry.qty) return "Insufficient godown stock";
              let newGodownQtys = Array.map(item.godownQtys, func(g2 : GodownQty) : GodownQty {
                if (g2.godownId == entry.fromId) { { godownId = g2.godownId; qty = g2.qty - entry.qty } }
                else g2
              });
              let updated : InventoryItem = {
                id = item.id; businessId = item.businessId; category = item.category;
                itemName = item.itemName; subCategory = item.subCategory;
                godownQtys = newGodownQtys; shopQty = item.shopQty + entry.qty;
                purchaseRate = item.purchaseRate; saleRate = item.saleRate;
              };
              inventory := Array.map(inventory, func(i : InventoryItem) : InventoryItem {
                if (i.id == item.id) updated else i
              });
            };
          };
        } else {
          if (item.shopQty < entry.qty) return "Insufficient shop stock";
          let newGodownQtys = Array.map(item.godownQtys, func(g : GodownQty) : GodownQty {
            if (g.godownId == entry.toId) { { godownId = g.godownId; qty = g.qty + entry.qty } }
            else g
          });
          let updated : InventoryItem = {
            id = item.id; businessId = item.businessId; category = item.category;
            itemName = item.itemName; subCategory = item.subCategory;
            godownQtys = newGodownQtys; shopQty = item.shopQty - entry.qty;
            purchaseRate = item.purchaseRate; saleRate = item.saleRate;
          };
          inventory := Array.map(inventory, func(i : InventoryItem) : InventoryItem {
            if (i.id == item.id) updated else i
          });
        };
        transfers := Array.append(transfers, [entry]);
        let tx : TxRecord = {
          id = entry.id; businessId = entry.businessId; txType = #transfer;
          biltyNumber = ""; category = entry.category; itemName = entry.itemName;
          subCategory = entry.subCategory;
          fromLocation = entry.fromType # ":" # entry.fromId;
          toLocation   = entry.toType   # ":" # entry.toId;
          transport = ""; qty = entry.qty; rate = entry.rate;
          enteredBy = entry.transferredBy; notes = "transfer"; createdAt = entry.createdAt;
        };
        txHistory := Array.append(txHistory, [tx]);
        return "ok";
      };
    };
  };

  public query func getDeliveries(businessId : Text) : async [DeliveryEntry] {
    Array.filter(deliveries, func(d : DeliveryEntry) : Bool { d.businessId == businessId })
  };

  public func addDelivery(entry : DeliveryEntry) : async Text {
    seed();
    for (item in entry.items.vals()) {
      let inv = Array.find(inventory, func(i : InventoryItem) : Bool {
        i.businessId == entry.businessId and i.category == item.category
          and i.itemName == item.itemName and i.subCategory == item.subCategory
      });
      switch (inv) {
        case null {};
        case (?existing) {
          let gq = Array.find(existing.godownQtys, func(g : GodownQty) : Bool { g.godownId == item.godownId });
          switch (gq) {
            case null {};
            case (?g) {
              if (g.qty < item.qty) return "Insufficient stock in godown";
              let newGodownQtys = Array.map(existing.godownQtys, func(g2 : GodownQty) : GodownQty {
                if (g2.godownId == item.godownId) { { godownId = g2.godownId; qty = g2.qty - item.qty } }
                else g2
              });
              let updated : InventoryItem = {
                id = existing.id; businessId = existing.businessId; category = existing.category;
                itemName = existing.itemName; subCategory = existing.subCategory;
                godownQtys = newGodownQtys; shopQty = existing.shopQty + item.qty;
                purchaseRate = existing.purchaseRate; saleRate = existing.saleRate;
              };
              inventory := Array.map(inventory, func(i : InventoryItem) : InventoryItem {
                if (i.id == existing.id) updated else i
              });
            };
          };
        };
      };
      let tx : TxRecord = {
        id = entry.id # "-" # item.itemName; businessId = entry.businessId; txType = #delivery;
        biltyNumber = entry.biltyNumber; category = item.category; itemName = item.itemName;
        subCategory = item.subCategory; fromLocation = item.godownId; toLocation = entry.customerName;
        transport = ""; qty = item.qty; rate = 0.0; enteredBy = entry.deliveredBy;
        notes = entry.customerPhone; createdAt = entry.createdAt;
      };
      txHistory := Array.append(txHistory, [tx]);
    };
    deliveries := Array.append(deliveries, [entry]);
    if (entry.deliveryType == "queue" and entry.biltyNumber != "") {
      queueEntries := Array.map(queueEntries, func(e : QueueEntry) : QueueEntry {
        if (e.biltyNumber == entry.biltyNumber) {
          { id = e.id; biltyNumber = e.biltyNumber; transport = e.transport;
            supplier = e.supplier; bales = e.bales; businessId = e.businessId;
            enteredBy = e.enteredBy; createdAt = e.createdAt; delivered = true }
        } else e
      });
    };
    "ok"
  };

  public query func getSales(businessId : Text) : async [SaleEntry] {
    Array.filter(sales, func(s : SaleEntry) : Bool { s.businessId == businessId })
  };

  public func addSale(entry : SaleEntry) : async Text {
    seed();
    for (item in entry.items.vals()) {
      let inv = Array.find(inventory, func(i : InventoryItem) : Bool {
        i.businessId == entry.businessId and i.category == item.category
          and i.itemName == item.itemName and i.subCategory == item.subCategory
      });
      switch (inv) {
        case null return "Item not found: " # item.itemName;
        case (?existing) {
          if (existing.shopQty < item.qty) return "Insufficient shop stock for: " # item.itemName;
          let updated : InventoryItem = {
            id = existing.id; businessId = existing.businessId; category = existing.category;
            itemName = existing.itemName; subCategory = existing.subCategory;
            godownQtys = existing.godownQtys; shopQty = existing.shopQty - item.qty;
            purchaseRate = existing.purchaseRate; saleRate = item.rate;
          };
          inventory := Array.map(inventory, func(i : InventoryItem) : InventoryItem {
            if (i.id == existing.id) updated else i
          });
        };
      };
      let tx : TxRecord = {
        id = entry.id # "-" # item.itemName; businessId = entry.businessId; txType = #sale;
        biltyNumber = ""; category = item.category; itemName = item.itemName;
        subCategory = item.subCategory; fromLocation = "Shop"; toLocation = "Customer";
        transport = ""; qty = item.qty; rate = item.rate; enteredBy = entry.recordedBy;
        notes = "sale"; createdAt = entry.createdAt;
      };
      txHistory := Array.append(txHistory, [tx]);
    };
    sales := Array.append(sales, [entry]);
    "ok"
  };

  public query func getTxHistory(businessId : Text) : async [TxRecord] {
    Array.filter(txHistory, func(t : TxRecord) : Bool { t.businessId == businessId })
  };

  public func addTxRecord(record : TxRecord) : async () {
    seed(); txHistory := Array.append(txHistory, [record]);
  };

  public func deleteTxRecord(id : Text) : async () {
    txHistory := Array.filter(txHistory, func(t : TxRecord) : Bool { t.id != id });
  };

  public func saveAppSettings(json : Text) : async () {
    appSettings := json;
  };

  public query func getAppSettings() : async Text {
    appSettings
  };

  public shared ({ caller }) func getCurrentUser() : async Text {
    caller.toText()
  };

};
