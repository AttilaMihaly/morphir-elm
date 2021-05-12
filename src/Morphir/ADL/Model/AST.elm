module Morphir.ADL.Model.AST exposing (Annotations, Ident, ModuleName, ScopedName)

import Dict exposing (Dict)
import Morphir.ADL.Model.Types exposing (..)


type alias Json =
    String


type alias ModuleName =
    String


type alias Ident =
    String


type alias Annotations =
    Map ScopedName Json


type alias ScopedName =
    { moduleName : ModuleName
    , name : Ident
    }


type TypeRef
    = Primitive Ident
    | TypeParam Ident
    | Reference ScopedName


type alias TypeExpr =
    { typeRef : TypeRef
    , parameters : List TypeExpr
    }


type alias Field =
    { name : Ident
    , serializedName : Ident
    , typeExpr : TypeExpr
    , default : Maybe Json
    , annotations : Annotations
    }


type alias Struct =
    { typeParams : List Ident
    , fields : List Field
    }


type alias Union =
    { typeParams : List Ident
    , fields : List Field
    }


type alias TypeDef =
    { typeParams : List Ident
    , typeExpr : TypeExpr
    }


type alias NewType =
    { typeParams : List Ident
    , typeExpr : TypeExpr
    , default : Maybe Json
    }


type DeclType
    = Struct_ Struct
    | Union_ Union
    | Type_ TypeDef
    | Newtype_ NewType


type alias Decl =
    { name : Ident
    , version : Maybe Int
    , type_ : DeclType
    , annotations : Annotations
    }


type alias ScopedDecl =
    { moduleName : ModuleName
    , decl : Decl
    }


type DeclVersions
    = List Decl


type Import
    = ModuleName ModuleName
    | ScopedName ScopedName


type alias Module =
    { name : ModuleName
    , imports : List Import
    , decls : Dict String Decl
    , annotations : Annotations
    }
