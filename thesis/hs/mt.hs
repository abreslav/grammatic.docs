import Data.List

data ModelTerm
  = Object {
      id :: ModelTerm,
      classRef :: ModelTerm,
      properties :: [Property]
    }
  | Ref { base :: ModelTerm }
  | List [ModelTerm]
  | Set [ModelTerm]
  | Null
  | Int Int
  | Bool Bool
  | Char Char
  | String String
  deriving (Eq)
  
data Property = Property {
    name :: ModelTerm,
    value :: ModelTerm
  }
  deriving (Eq)

pSubterms :: Property -> [ModelTerm]
pSubterms (Property n v) = (subterms n) ++ (subterms v)

subterms :: ModelTerm -> [ModelTerm]
subterms o = o : case o of
  Object id cr p -> (subterms id) ++ (subterms cr) ++ (concat (map pSubterms p))
  Ref    b       -> subterms b
  List   l       -> concat (map subterms l)
  Set    l       -> concat (map subterms l)
  _              -> []  

type RContext = [(ModelTerm, ModelTerm)]

rcontext :: ModelTerm -> RContext
rcontext t = map toPair (filter isObject (subterms t))
  where
    toPair o@(Object id _ _) = (id, o)

    isObject (Object _ _ _) = True
    isObject _              = False

data Class = Class {
    abstract :: Bool, 
    class_id :: String, 
    superclasses :: [Class], 
    propertyDescriptors :: [PropertyDescriptor]
  }

data PropertyDescriptor = PropertyDescriptor String Type

data Enum = Enum {
    enum_id :: String,
    literals :: [String]
  }

data Type
  = CharT
  | StringT
  | IntT
  | BoolT
  | NullableT Type
  | SetT Type Bool
  | ListT Type Bool
  | ValT Class
  | RefT Class
  | EnumT Main.Enum
  | TopT

extractClasses :: ModelTerm -> [Class]
extractClasses t@(Set objects) = map (extractClass (rcontext t)) objects
  where
    extractClass :: RContext -> ModelTerm -> Class
    extractClass context (Object (String id) (Ref (String "Class")) props) 
           = Class 
               (extractAbstract props) 
               id 
               (concat (map (superclasses context) props)) 
               (concat (map (toDescriptors context) props))

    extractAbstract :: [Property] -> Bool
    extractAbstract l = head (concat (map isAbstract l))

    isAbstract :: Property -> [Bool] 
    isAbstract (Property (Ref (String "Class.abstract")) (Bool v)) = [v]
    isAbstract _                                      = []

    superclasses :: RContext -> Property -> [Class]
    superclasses context (Property (Ref (String "Class.superclasses")) (Set refs)) = map (toClass context) refs
    superclasses _ _ = []

    toClass :: RContext -> ModelTerm -> Class
    toClass context (Ref id) = case lookup id context of Just a -> extractClass context a
    toClass _ r = Class False ("E: " ++ (show r)) [] []

    toDescriptors :: RContext -> Property -> [PropertyDescriptor]
    toDescriptors context (Property (Ref (String "Class.propertyDescriptors")) (Set s)) = map (toDescriptor context) s
    toDescriptors _ _ = []

    toDescriptor :: RContext -> ModelTerm -> PropertyDescriptor
    toDescriptor context (Object (String id) (Ref (String "PropertyDescriptor")) [Property (Ref (String "PropertyDescriptor.type")) propType]) = PropertyDescriptor id (toType context propType)

    toType :: RContext -> ModelTerm -> Type
    toType context (Object _ (Ref (String "PrimitiveType")) [(Property (Ref (String "PrimitiveType.type")) (String "Boolean"))]) = BoolT
    toType context (Object _ (Ref (String "PrimitiveType")) [(Property (Ref (String "PrimitiveType.type")) (String "String"))]) = StringT
    toType context (Object _ (Ref (String "PrimitiveType")) [(Property (Ref (String "PrimitiveType.type")) (String "Integer"))]) = IntT
    toType context (Object _ (Ref (String "PrimitiveType")) [(Property (Ref (String "PrimitiveType.type")) (String "Char"))]) = CharT

    toType context (Object _ (Ref (String "NullableType")) [(Property (Ref (String "NullableType.type")) body)]) = NullableT (toType context body)
    toType context (Object _ (Ref (String "ReferenceType")) [(Property (Ref (String "ClassType.class")) (classRef))]) = RefT (toClass context classRef)
    toType context (Object _ (Ref (String "ObjectType")) [(Property (Ref (String "ClassType.class")) (classRef))]) = ValT (toClass context classRef)

    toType context (Object _ (Ref (String "SetType")) [
         (Property (Ref (String "CollectionType.nonEmpty")) (Bool nonEmpty)),
         (Property (Ref (String "CollectionType.elementType")) elementType)
      ]) = SetT (toType context elementType) nonEmpty
    toType context (Object _ (Ref (String "ListType")) [
         (Property (Ref (String "CollectionType.nonEmpty")) (Bool nonEmpty)),
         (Property (Ref (String "CollectionType.elementType")) elementType)
      ]) = ListT (toType context elementType) nonEmpty

    toType context 
        (Object 
            _ 
            (Ref (String "EnumType")) 
            [Property 
                (Ref (String "EnumType.enum"))
                (Ref enumId)]) = EnumT (toEnum (lookup enumId context))

    toType context (Object _ (Ref (String "AnyType")) []) = TopT
    toType context _ = TopT

    toEnum :: Maybe ModelTerm -> Main.Enum
    toEnum (Just 
        (Object 
            (String id)
            (Ref (String "Enum"))
            [Property 
                (Ref (String "Enum.literals"))
                (List literals)])) = Enum id (map toLiteral literals)

    toLiteral :: ModelTerm -> String
    toLiteral 
        (Object 
            (String id) 
            (Ref (String "EnumLiteral")) 
            []) = id

instance Show Property where
  show (Property name value) = (show name) ++ " = " ++ (show value)

instance Show ModelTerm where
  show (String s) = if (elemIndices ' ' s) == [] then s else "'" ++ s ++ "'"
  show (Char c) = show c
  show (Bool b) = show b
  show (Int i) = show i
  show (Null) = "Null"
  show (Set l) = "{" ++ (csl l) ++ "}"
  show (List l) = show l
  show (Ref r) = "@" ++ (show r)
  show (Object id cr props) = "object " ++ (show id) ++ " : " ++ (show cr) ++ (" {" ++ (csl props) ++ "}")

instance Show Type where
  show CharT = "Char"
  show StringT = "String"
  show IntT = "Int"
  show BoolT = "Boolean"
  show (NullableT t) = (show t) ++ "?"
  show (SetT t nonEmpty) = "{" ++ (show t) ++ (if nonEmpty then "+" else "*") ++ "}"
  show (ListT t nonEmpty) = "[" ++ (show t) ++ (if nonEmpty then "+" else "*") ++ "]"
  show (ValT (Class _ n _ _)) = "val(" ++ n ++ ")"
  show (RefT (Class _ n _ _)) = "ref(" ++ n ++ ")"
  show TopT = "Any"

instance Show Class where
  show (Class a n s p) = "\n" ++ (if a then "abstract " else "") ++ "class " ++ n ++ (if isEmpty s then "" else " : " ++ (css (map className s))) ++ " {" ++ (css (map ((++) "\n  ".show) p)) ++ "\n}"
    where className (Class _ n _ _) = n

instance Show PropertyDescriptor where
  show (PropertyDescriptor n t) = n ++ " : " ++ (show t)

isEmpty [] = True
isEmpty _  = False

csl :: Show a => [a] -> String
csl l = css (map show l)
--csl l = tail (foldr (\mt s -> ',':(show mt) ++ s) [] l)

css :: [String] -> String
css [] = ""
css l = tail (foldr (\mt s -> ',':mt ++ s) [] l)

printMT :: ModelTerm -> String
printMT (Object id cr props) = ""

sample = 
  Set [
    Object (String "Class") (Ref (String "Class")) [
        Property (Ref (String "abstract")) (Bool True),
        Property (Ref (String "superclasses")) (List []),
        Property (Ref (String "properties")) (Set [
            Object (String "abstract") (Ref (String "Property")) [ 
                Property (Ref (String "type")) (Object (Int 1) (Ref (String "PrimitiveType")) [])
            ]
        ])
    ]
  ]


  
sampleClass = Class True "Class" [] [
        PropertyDescriptor "p" (ListT (ValT sampleClass) True)
    ]

longSample = (Set [
    (Object (String "Type") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool True),
      Property (Ref (String "Class.superclasses")) (Set []),
      Property (Ref (String "Class.propertyDescriptors")) (Set [])
    ]),
    (Object (String "AnyType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "Type"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [])
    ]),
    (Object (String "ClassType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool True),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "Type"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "ClassType.class") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 0)]) (Ref (String "ReferenceType")) [
            Property (Ref (String "ClassType.class")) (Ref (String "Class"))
          ])
        ])
      ])
    ]),
    (Object (String "NullableType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "Type"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "NullableType.type") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 1)]) (Ref (String "ObjectType")) [
            Property (Ref (String "ClassType.class")) (Ref (String "Type"))
          ])
        ])
      ])
    ]),
    (Object (String "ReferenceType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "ClassType"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [])
    ]),
    (Object (String "ObjectType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "ClassType"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [])
    ]),
    (Object (String "PrimitiveType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "Type"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "PrimitiveType.type") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 2)]) (Ref (String "PrimitiveType")) [
            Property (Ref (String "PrimitiveType.type")) (String "String")
          ])
        ])
      ])
    ]),
    (Object (String "CollectionType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool True),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "Type"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "CollectionType.nonEmpty") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 3)]) (Ref (String "PrimitiveType")) [
            Property (Ref (String "PrimitiveType.type")) (String "Boolean")
          ])
        ]),
        (Object (String "CollectionType.elementType") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 4)]) (Ref (String "ObjectType")) [
            Property (Ref (String "ClassType.class")) (Ref (String "Type"))
          ])
        ])
      ])
    ]),
    (Object (String "SetType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "CollectionType"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [])
    ]),
    (Object (String "ListType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "CollectionType"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [])
    ]),
    (Object (String "EnumType") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set [
        (Ref (String "Type"))
      ]),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "EnumType.enum") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 5)]) (Ref (String "ReferenceType")) [
            Property (Ref (String "ClassType.class")) (Ref (String "Enum"))
          ])
        ])
      ])
    ]),
    (Object (String "PropertyDescriptor") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set []),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "PropertyDescriptor.type") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 6)]) (Ref (String "ObjectType")) [
            Property (Ref (String "ClassType.class")) (Ref (String "Type"))
          ])
        ])
      ])
    ]),
    (Object (String "Enum") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set []),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "Enum.literals") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 7)]) (Ref (String "SetType")) [
            Property (Ref (String "CollectionType.nonEmpty")) (Bool False),
            Property (Ref (String "CollectionType.elementType")) (Object (List [(String "MMM.xml"), (Int 8)]) (Ref (String "ObjectType")) [
              Property (Ref (String "ClassType.class")) (Ref (String "EnumLiteral"))
            ])
          ])
        ])
      ])
    ]),
    (Object (String "EnumLiteral") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set []),
      Property (Ref (String "Class.propertyDescriptors")) (Set [])
    ]),
    (Object (String "Class") (Ref (String "Class")) [
      Property (Ref (String "Class.abstract")) (Bool False),
      Property (Ref (String "Class.superclasses")) (Set []),
      Property (Ref (String "Class.propertyDescriptors")) (Set [
        (Object (String "Class.abstract") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 9)]) (Ref (String "PrimitiveType")) [
            Property (Ref (String "PrimitiveType.type")) (String "Boolean")
          ])
        ]),
        (Object (String "Class.superclasses") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 10)]) (Ref (String "SetType")) [
            Property (Ref (String "CollectionType.nonEmpty")) (Bool False),
            Property (Ref (String "CollectionType.elementType")) (Object (List [(String "MMM.xml"), (Int 11)]) (Ref (String "ReferenceType")) [
              Property (Ref (String "ClassType.class")) (Ref (String "Class"))
            ])
          ])
        ]),
        (Object (String "Class.propertyDescriptors") (Ref (String "PropertyDescriptor")) [
          Property (Ref (String "PropertyDescriptor.type")) (Object (List [(String "MMM.xml"), (Int 12)]) (Ref (String "SetType")) [
            Property (Ref (String "CollectionType.nonEmpty")) (Bool False),
            Property (Ref (String "CollectionType.elementType")) (Object (List [(String "MMM.xml"), (Int 13)]) (Ref (String "ObjectType")) [
              Property (Ref (String "ClassType.class")) (Ref (String "PropertyDescriptor"))
            ])
          ])
        ])
      ])
    ])
  ])


main = do 
  putStrLn (show (extractClasses longSample))
  putStrLn (show sampleClass)
