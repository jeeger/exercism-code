module School (School, add, empty, grade, sorted) where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid

newtype Grade = Grade { getGrade:: Int } deriving (Show, Eq, Ord)
newtype Student = Student { getName :: String} deriving (Show, Eq, Ord)
newtype School = School {getMap:: M.Map Grade (S.Set Student)} deriving (Show)

instance Monoid School where
  mempty = School M.empty
  mappend (School map1) (School map2) = School $ M.unionWith (<>) map1 map2

add :: Int -> String -> School -> School
add gradeNum name school = let grade =  (Grade gradeNum)
                               student = (Student name)
  in school <> (School (M.singleton grade (S.singleton student)))
    
empty :: School
empty = mempty

grade :: Int -> School -> [String]
grade gradeNum (School map) = let grade = (Grade gradeNum)
                              in fmap getName $ S.elems (M.findWithDefault S.empty grade map)

sorted :: School -> [(Int, [String])]
sorted (School m) = fmap (\(g, v) -> (getGrade g, fmap getName (S.elems v))) (M.toList m)
