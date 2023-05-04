require "test_helper"

class BandsControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    travel_to Date.new(2022, 3, 20)

    gd = create(:band, id: "1585f2eb-0c0a-5ba7-a747-9f45120d2c02", collection: "GratefulDead", name: "Grateful Dead", logo_url: "https://google.com")
    create_list(:performance, 2, band: gd)
    create_list(:recording, 3, performance: gd.performances.first)

    bs = create(:band, id: "cb60917e-f370-5f51-98b9-9184293718d5", collection: "BillyStrings", name: "Billy String", logo_url: "https://yahoo.com")
    create_list(:performance, 1, band: bs)
    create_list(:recording, 1, performance: bs.performances.first)

    get bands_url(format: :json)
    assert_response :success

    expected = [
      {
        id: "cb60917e-f370-5f51-98b9-9184293718d5",
        collection: "BillyStrings",
        name: "Billy String",
        logo_url: "https://yahoo.com",
        num_performances: 1,
        num_recordings: 1,
        created_at: "2022-03-20T00:00:00.000Z",
        updated_at: "2022-03-20T00:00:00.000Z",
      },
      {
        id: "1585f2eb-0c0a-5ba7-a747-9f45120d2c02",
        collection: "GratefulDead",
        name: "Grateful Dead",
        logo_url: "https://google.com",
        num_performances: 2,
        num_recordings: 3,
        created_at: "2022-03-20T00:00:00.000Z",
        updated_at: "2022-03-20T00:00:00.000Z",
      }
    ]

    assert_equal expected, JSON.parse(response.body).map(&:deep_symbolize_keys)
  end
end
