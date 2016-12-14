(ns untangled-template.intro
  (:require [devcards.core :as rc :refer-macros [defcard defcard-doc]]
            [om.next :as om :refer-macros [defui]]
            [untangled-template.ui.components :as comp]
            [om.dom :as dom]
            [devcards.core :as dc]))

;; assuming SELECT p.id AS person/id, ... LEFT JOIN phone ...
(def person-rows
  [{:person/id 2 :person/name "Sally"}
   {:person/id 1 :person/name "Joe" :phone/id 1 :phone/type "home" :phone/number "555-1212" :billing/id 1 :billing/cc "3333-4444-5555-1111"}
   ; Not sure if the columns will show up as nil or just missing on a left join
   {:person/id 1 :person/name "Joe" :phone/id 2 :phone/type "work" :phone/number "555-1212" :billing/id 1 :billing/cc "3333-4444-5555-1111"}])

(def codecs
  {:phone/type {:in  keyword
                :out name}})

(defn decode-row [codecs row]
  (reduce (fn [row [k v]]
            (if-let [codec (get-in codecs [k :in])]
              (assoc row k (codec v))
              row)) row row))

(def decoded-rows (map (partial decode-row codecs) person-rows))

; TODO: We can derive this from the row data, but be careful, might have to scan all rows. Might be too inefficient. Might
; still want to ask user to supply.
(def possible-entities #{:person :phone :billing})

(defn build-ident [kw row] [kw (get row kw)])

(defn get-entity [ns row]
  (let [ns (name ns)]
    (reduce (fn [entity [k v]]
              (if (= ns (namespace k))
                (assoc entity k v)
                entity)) {} row)))

(def sample-entity (get-entity :person (first decoded-rows)))

(defn derive-ident
  "Looks at a given map with possible ident IDs. Returns the first ident it can find
  that has a non-nil ID.

  Returns [ident entity-with-db-id], or nil if none are found."
  [entity possible-ns]
  (let [possible-ids (map #(keyword (name %) "id") possible-ns)
        all-idents (map (fn [kw] (build-ident kw entity)) possible-ids)
        valid-idents (keep (fn [[k id]] (when-not (nil? id)
                                          [k id])) all-idents)
        ident (first valid-idents)
        id-key (first ident)
        id (second ident)
        entity (when ident
                 (-> entity (dissoc id-key)
                     (assoc :db/id id)))]
    (when entity
      [ident entity])))

(def sample-entity-derived-with-ident (derive-ident sample-entity possible-entities))

(def first-decoded-row (first decoded-rows))

(defn entities-in-row
  "Find all entities in a row, and return this as a sequence of pairs: [ident entity].

  possible-entities can be a keyword or string to represent the namespace."
  [row possible-entities]
  (let [entities (map #(get-entity % row) possible-entities)
        result (keep #(derive-ident % possible-entities) entities)]
    result))

(def sample-entities (entities-in-row first-decoded-row possible-entities))
(def om-style-table (reduce (fn [graph-db [ident entity]] (assoc-in graph-db ident entity)) {} sample-entities))

(def all-entities (reduce (fn [entities row]
                            (into entities (entities-in-row row possible-entities))) #{} decoded-rows))

(def om-result
  (reduce (fn [graph-db [ident entity]] (assoc-in graph-db ident entity)) {} all-entities))

(defn decode-graph-edge
  "Add a graph edge to the given graph-db (which must already have the entities in it).

  row is a row of the result set
  from is the property on the entity that should point towards another entity
  to is the property on the result row that contains the ID of the target
  arity is :many or :one

  Returns a new graph-db with the edge in place.
  "
  [graph-db row from to arity]
  (let [from-type (namespace from)
        from-id-kw (keyword from-type "id")
        from-id (get row from-id-kw)
        from-ident [from-id-kw from-id]
        to-id (get row to)
        to-ident [to to-id]
        valid-idents? (and to-id from-id)]
    (if valid-idents?
      (if (= :many arity)
        ; FIXME: HIGHLY INEFFICIENT (comp vec distinct conj)
        (update-in graph-db from-ident update from (fnil (comp vec distinct conj) []) to-ident)
        (update-in graph-db from-ident assoc from to-ident))
      graph-db)))

(def sample-single-decode (decode-graph-edge om-result first-decoded-row :person/phone :phone/id :many))

(def graph-edge-defs [
                      [:person/phone :phone/id :many]
                      [:person/billing :billing/id :one]
                      ])

(defn decode-graph-edges-on-row
  [graph-db row graph-edges]
  (reduce (fn [db edge] (apply decode-graph-edge db row edge)) graph-db graph-edges))

(defn decode-graph-edges
  [graph-db rows graph-edges]
  (reduce (fn [db row] (decode-graph-edges-on-row db row graph-edges)) graph-db rows))

(def final-result (decode-graph-edges om-result decoded-rows graph-edge-defs))

(defn all-idents
  "Returns all of the idents for a given table (e.g. :person/id)"
  [graph-db table]
  (let [ids (keys (get graph-db table))]
    (mapv #(vector table %) ids)))

(def hacked-result (assoc final-result :people (all-idents final-result :person/id)))

(def query [{:people [:db/id :person/name
                      {:person/phone [:db/id :phone/number :phone/type]}
                      {:person/billing [:db/id :billing/cc]}]}])

(defcard-doc
  "# Converting a table to a tree

  Given a SELECT where the AS clause is used to rename columns to something as close as
  possible to namespaced keywords:

  SELECT p.id AS person/id ... LEFT JOIN phone ...

  returns something like (not sure if left join nils come across or are just elided. doesn't matter):
  "
  (dc/mkdn-pprint-source person-rows)

  "we can first make a pass at converting the column values to their appropriate type. For
  example if we'd like `:phone/type` to be a keyword, we could define a codec set as:
  "
  (dc/mkdn-pprint-source codecs)
  "and can do a very simple (and reusable) sequence of operations:"
  (dc/mkdn-pprint-source decode-row)
  (dc/mkdn-pprint-source decoded-rows)
  "which will result in the following result rows:"
  decoded-rows

  "
  ## Extracting the entities

  Now we note that for each row, there may be one or more new entities represented by that row,
  and that each row does in fact imply relation. We can use tricks from Om to convert these
  row entities into the kinds of tables we use on the client side (e.g. via some ident function)

  We can also know which attributes belong to a given entity just by looking at the namespaces.

  So, we can first extract an entity like so:
  "
  (dc/mkdn-pprint-source get-entity)
  "which could be used like this: "
  (dc/mkdn-pprint-source sample-entity)
  "to produce:"
  sample-entity
  "## Putting them in tables (ala Om)

  Now we can start to construct our own graph database from the result. First
  we need the concept of ident:
  "
  (dc/mkdn-pprint-source build-ident)
  "If we know what kinds of entities are possible (by their ID keyword):"
  (dc/mkdn-pprint-source possible-entities)
  "Then we can combine these to get a valid ident and at the same time rename the ID field
  to `:db/id`:"
  (dc/mkdn-pprint-source derive-ident)
  (dc/mkdn-pprint-source sample-entity-derived-with-ident)
  sample-entity-derived-with-ident
  "
  Now we can combine the above to pull all entities out of a given row:

  "
  first-decoded-row
  (dc/mkdn-pprint-source entities-in-row)
  sample-entities
  "Resulting in an om-style table."
  om-style-table
  "Completing the entire om-style databae is just doing the same process across all rows in the result set.

  NOTE: we can short-circuit steps where the entity already exists in the result (dupe data in the result set
  from the nature of SQL). (TODO)"
  all-entities
  "resulting in:"
  om-result

  "# Generating the graph edges

  So if we re-walk the result-set, and find valid IDs that represent a graph edge, we can use our knowledge of that
  edge to update the graph database to include the edges:
 "
  (dc/mkdn-pprint-source decode-graph-edge)
  (dc/mkdn-pprint-source sample-single-decode)
  sample-single-decode
  "Once we can do a single graph edge, we can walk the entire result set and all edges to complete the graph."
  (dc/mkdn-pprint-source decode-graph-edges-on-row)
  (dc/mkdn-pprint-source decode-graph-edges)
  (dc/mkdn-pprint-source graph-edge-defs)
  (dc/mkdn-pprint-source final-result)
  final-result
  "and from there, we can hack the db to match up to an Om query and use Om `db->tree` to make our result to send
  to the client: (assoc some fake key at top or something)"
  (dc/mkdn-pprint-source hacked-result)
  hacked-result
  query
  "So that we have something to send back to the client from the server:"
  (om/db->tree query hacked-result hacked-result)

  "# Combining it all together

  We notice that we can derive the entities from the namespaces on the result set (might have to walk all rows for that).

  So, the only data we MUST supply to this algorithm (besides the row data) is the UI query we're trying to satisfy, and
  the row-level graph edges that are implied in the model.

  TODO: function to do a final clojure.set/rename-keys on entities before response (spectre with transform)
  TODO: top-level function to combine all steps
  "
  )
