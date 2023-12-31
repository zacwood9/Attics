class HomeController < ApplicationController
  def index
    render 'welcome/index', layout: 'application'
  end
end
