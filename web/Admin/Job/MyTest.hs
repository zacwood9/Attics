module Admin.Job.MyTest where
import Admin.Controller.Prelude

instance Job MyTestJob where
    perform MyTestJob { .. } = do
        putStrLn "Hello World!"
