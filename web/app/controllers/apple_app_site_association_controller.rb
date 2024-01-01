# frozen_string_literal: true

class AppleAppSiteAssociationController < ApplicationController
  def index
    render json: {
      applinks: {
        apps: [],
        details: [
          {
            appID: "W9S2BXPP37.me.zacwood.Attics",
            paths: [ "*" ]
          }
        ]
      }
    }
  end
end
