[file
  [import "base.dsl"]
  [import "dsl/types.dsl"]
  [import "dsl/types_utils.dsl"]

  [class-decl XDef]
  [class-struct XDef
    [field uid UID]
    [field name Name]
    [field is_auto Bool]
    [field annotations [Array String]]
    [field ty Ty] // Must have kind '*'.
    [field tm [Mut [Maybe Tm]]] // 'None' if has not been implemented yet.
  ]

  // Creates a 'XDef' with:
  // - 'is_auto' defaulted to 'false' (you can change this with `.=is_auto`)
  // - empty 'annotations'
  [def-decl pub XDef.new_simple* [-> [Dict [uid UID] [name Name] [ty Ty] [tm Tm]] XDef]]
  [def-impl XDef.new_simple* [\ d
    [XDef.new* [dict
      [uid [d ./ uid]]
      [name [d ./ name]]
      [is_auto false]
      [annotations [Array.create ,]]
      [ty [d ./ ty]]
      [tm [var [Some [d ./ tm]]]]
    ]]
  ]]

  [class-decl XTyDef]
  [class-struct XTyDef
    [field uid UID]
    [field name Name]
    [field kind Kind]
    [field ty Ty]
  ]

  [class-decl XMember]
  [class-struct XMember
    [field id MemberID]
    [field name Name]
    [field ty Ty] // Value type. Scoped under the enum's generics.
  ]

  [class-decl XClass]
  [class-struct XClass
    [field uid UID]
    [field name Name]
    [field generics TyScope]

    // Empty if not implemented.
    //
    // Each member's ID corresponds to its index in this array.
    [field members [Array XMember]]
  ]

  [def-decl pub XClass.get_member_by_name? [-> XClass Name [Maybe XMember]]]
  [def-impl XClass.get_member_by_name? [\ self name [do
    // TODO: Optimize with a StringMap
    [self . XClass.members . Array.find [\ member [member . XMember.name . == name]]]
  ]]]

  [def-decl pub XClass.get_member_by_id? [-> XClass MemberID [Maybe XMember]]]
  [def-impl XClass.get_member_by_id? [\ self id [do
    [self . XClass.members . Array.at? id]
  ]]]


  [def-decl pub XClass.compute_kind [-> XClass Kind]]
  [def-impl XClass.compute_kind [\ self [do
    [self . XClass.generics . TyScope.fold_as_kind_arr Kind.Star]
  ]]]

  [class-decl XEntry]
  [class-enum XEntry
    [member Def XDef]
    [member TyDef XTyDef]
    [member Class XClass]
  ]

  [def-decl XEntry.uid [-> XEntry UID]]
  [def-impl XEntry.uid [\ entry [do
    [match entry
      [[Def def] [def . XDef.uid]]
      [[TyDef tydef] [tydef . XTyDef.uid]]
      [[Class class] [class . XClass.uid]]
    ]
  ]]]

  // Returns the name of XEntry that is suitable for error messages,
  // prettyprinting and debugging.
  //
  // Should not be used for identfiying entries.
  [def-decl pub XEntry.name [-> XEntry Name]]
  [def-impl XEntry.name [\ entry [do
    [match entry
      [[Def def] [def . XDef.name]]
      [[TyDef tydef] [tydef . XTyDef.name]]
      [[Class class] [class . XClass.name]]
    ]
  ]]]

  [class-decl Database]
  [class-struct Database
    [field entries [I32MutMap XEntry]]
    [field on_fresh_uid [-> , UID]]
  ]

  [def-decl pub Database.create [-> , Database]]
  [def-impl Database.create [\ _ [do
    [let counter [var 0]]
    [Database.new* [dict
      [entries [I32MutMap.create ,]]
      [on_fresh_uid [\ _ [do
        [let uid [get counter]]
        [set counter [I32.suc uid]]
        uid
      ]]]
    ]]
  ]]]

  [def-decl pub Database.fresh_uid [-> Database UID]]
  [def-impl Database.fresh_uid [\ self [do
    [self . Database.on_fresh_uid ,]
  ]]]

  [def-decl pub Database.insert_entry [-> Database XEntry ,]]
  [def-impl Database.insert_entry [\ self entry [do
    [self . Database.entries . I32MutMap.set [entry . XEntry.uid] entry]
  ]]]

  [def-decl pub Database.get_entry [-> Database UID XEntry]]
  [def-impl Database.get_entry [\ self uid [do
    [self . Database.entries . I32MutMap.get uid]
  ]]]

  [def-decl pub Database.get_def [-> Database UID XDef]]
  [def-impl Database.get_def [\ self uid [do
    [match [self . Database.get_entry uid]
      [[Def def] def]
      [_ [panic "Not a Def"]]
    ]
  ]]]

  [def-decl pub Database.get_tydef [-> Database UID XTyDef]]
  [def-impl Database.get_tydef [\ self uid [do
    [match [self . Database.get_entry uid]
      [[TyDef tydef] tydef]
      [_ [panic "Not a TyDef"]]
    ]
  ]]]

  [def-decl pub Database.get_class [-> Database UID XClass]]
  [def-impl Database.get_class [\ self uid [do
    [match [self . Database.get_entry uid]
      [[Class class] class]
      [_ [panic "Not a Class"]]
    ]
  ]]]

  [def-decl pub Database.iter_def_uids [-> Database [Iter UID]]]
  [def-impl Database.iter_def_uids [\ self [do
    [self
      . Database.entries
      . I32MutMap.iter_values
      . Iter.map_maybe [\ entry [do
        [match entry
          [[Def def] [Some [def . XDef.uid]]]
          [_ [None]]
        ]
      ]]
    ]
  ]]]

  [def-decl pub Database.iter_auto_def_uids [-> Database [Iter UID]]]
  [def-impl Database.iter_auto_def_uids [\ self [do
    // TODO: OPTIMIZE. Add filtering by type. Add lookup indexes for speed. This
    // is used by instance resolution.
    [self
      . Database.entries
      . I32MutMap.iter_values
      . Iter.map_maybe [\ entry [do
        [else-let [Def def] entry
          [None]]
        [try _ [Maybe.guard [def . XDef.is_auto]]]
        [pure [def . XDef.uid]]
      ]]
    ]
  ]]]
]