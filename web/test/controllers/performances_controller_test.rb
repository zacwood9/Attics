require "test_helper"

class PerformancesControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    band = create(:band)
    barton = create(:performance, band: band, date: "1977-05-08")
    create(:recording, performance: barton, avg_rating: 5, num_reviews: 1)
    create(:recording, performance: barton, avg_rating: 4.5, num_reviews: 2)

    _other = create(:performance, band: band, date: "1979-05-09")

    get band_performances_url(collection: band.collection, year: "1977", format: :json)
    assert_response :ok
    body = JSON.parse(response.body)
    assert_equal 1, body["performances"].length
  end

  test "top performances should sort by star rating" do
    band = create(:band)
    barton = create(:performance, band: band, date: "1977-05-08")
    create(:recording, performance: barton, avg_rating: 5, num_reviews: 1)
    create(:recording, performance: barton, avg_rating: 4.5, num_reviews: 2)

    ithica = create(:performance, band: band, date: "1977-05-09")
    create(:recording, performance: ithica, avg_rating: 4, num_reviews: 1)
    create(:recording, performance: ithica, avg_rating: 3.5, num_reviews: 2)

    get band_top_performances_url(
          collection: band.collection,
          format: :json
        )

    assert_response :success
    body = JSON.parse(response.body)

    assert_equal "1977-05-08", body.dig("top_performances", "1977", 0, "date")
    assert_equal "1977-05-09", body.dig("top_performances", "1977", 1, "date")
  end

  test "top performances should include on_this_day" do
    travel_to Date.new(2023, 5, 8)

    band = create(:band)
    barton = create(:performance, band: band, date: "1977-05-08")
    create(:performance, band: band, date: "1977-05-09")
    create(:recording, performance: barton, avg_rating: 5, num_reviews: 1)

    get band_top_performances_url(
          collection: band.collection,
          on_this_day: "2022-05-08",
          format: :json
        )

    assert_response :success
    body = JSON.parse(response.body)

    assert_equal "1977-05-08", body["on_this_day"][0]["date"]
  end

  test "top performances should return empty on_this_day if not passed param" do
    travel_to Date.new(2023, 5, 8)

    band = create(:band)
    barton = create(:performance, band: band, date: "1977-05-08")
    create(:performance, band: band, date: "1977-05-09")
    create(:recording, performance: barton, avg_rating: 5, num_reviews: 1)

    get band_top_performances_url(
          collection: band.collection,
          format: :json
        )

    assert_response :success
    body = JSON.parse(response.body)

    assert_equal [], body["on_this_day"]
  end
end
