module Admin.Controller.Static where
import Admin.Controller.Prelude
import Admin.View.Static.Welcome

instance Controller StaticController where
    action WelcomeAction = render WelcomeView
