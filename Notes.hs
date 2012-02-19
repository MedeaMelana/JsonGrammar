data JsonError
  = ExpectedProperty Text
  | UnexpectedProperty
  | ExpectedArray
  | ExpectedObject
  | ExpectedLiteral Value
  | UnknownError

data PathSegment = ArrayIndex Int | ObjectProperty Text

type Path = [PathSegment]

descend :: PathSegment -> Iso a b -> Iso a b
descend = undefined

throw :: JsonError -> Iso a b
throw = undefined

-- type ErrorTree = Tree


{ 'aap' : 5 }

array id <> object (fixedProp "aap" 6 <> fixedProp "aap" 7)

([], ExpectedArray)
([ObjectProperty "aap"], ExpectedLiteral (Number 6))
([ObjectProperty "aap"], ExpectedLiteral (Number 7))



array id
<>
object (propBy (array (id <> elementBy (litJson 8))) "foo" . (fixedProp "bar" 6 <> fixedProp "bar" 7))

{ foo : [1], bar : 5 }

([], ExpectedArray)
([ObjectProperty "foo"], ExpectedEmptyArray)
([ObjectProperty "bar"], ExpectedLiteral (Number 6))
([ObjectProperty "bar"], ExpectedLiteral (Number 7))



data ErrorTree = ErrorTree [JsonError] (Map PathSegment ErrorTree)

type JsonErrorM = Either [(Path, JsonError)]
