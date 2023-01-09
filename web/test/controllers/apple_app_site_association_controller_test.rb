require "test_helper"

class AppleAppSiteAssociationControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    get "/.well-known/apple-app-site-association"
    assert_response :success

    expected = {
      applinks: {
        apps: [],
        details: [
          {
            appID: "W9S2BXPP37.me.zacwood.Attics",
            paths: ["*"]
          }
        ]
      }
    }

    assert_equal expected, JSON.parse(response.body).deep_symbolize_keys
  end
end
