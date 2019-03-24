module Student where


data Student = Student
              { name::String
              , surname::String
              , index::Int
              } deriving(Eq, Show)


author = Student "Adrian" "Janakiewicz" 213456


someFunc :: IO ()
someFunc = print author
