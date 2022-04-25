module Select where
import Grammar
import Utilities

select :: Attribute -> [Triple] -> [Triple]
select (Attributes x y) t = select x t ++ select y t
select Subj t = undefined--list comp
--