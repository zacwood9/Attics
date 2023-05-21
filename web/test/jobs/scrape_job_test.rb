require "test_helper"

class ScrapeJobTest < ActiveJob::TestCase
  test "inserts and updates" do
    stub_request(:get, "https://archive.org/services/search/v1/scrape?fields=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier&q=collection:GratefulDead")
      .to_return(body: FIRST_JSON)
    stub_request(:get, "https://archive.org/services/search/v1/scrape?fields=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier&q=collection:GratefulDead&cursor=cursor")
      .to_return(body: SECOND_JSON)
    stub_request(:get, "https://archive.org/metadata/gd78-05-08.sbd.hicks.4982.sbeok.shnf/files")
      .to_return(body: TRACKS_JSON)

    recording = recordings(:grateful_dead_1977_05_08_hicks_sbd)

    assert_difference %w[Performance.count Recording.count], 1 do
      assert_difference 'Track.count', 2 do
        ScrapeJob.perform_now bands(:grateful_dead)
      end
    end

    assert_equal 80431, recording.reload.archive_downloads
    assert_equal "Soundboard", recording.source
    assert_equal "lineage", recording.lineage

    new_recording = Recording.find_by!(identifier: "gd78-05-08.sbd.hicks.4982.sbeok.shnf")
    assert_equal "North Pole", new_recording.performance.city
    assert_equal "SD", new_recording.performance.state
    assert_equal "Candy Land", new_recording.performance.venue

    mp3 = new_recording.tracks.find(&:mp3?)
    assert_predicate mp3, :present?
    assert_equal "06:10", mp3.length
    assert_equal 2, mp3.track
    assert_equal "Saturday Night", mp3.title

    flac = new_recording.tracks.find(&:flac?)
    assert_predicate flac, :present?
    assert_equal "06:10", flac.length
    assert_equal 2, flac.track
    assert_equal "Saturday Night", flac.title
  end

  private

  FIRST_JSON = <<~JSON.freeze
    {
      "items": [
        {
          "date": "1977-05-08T00:00:00Z",
          "coverage": "Ithica, NY",
          "identifier": "gd77-05-08.sbd.hicks.4982.sbeok.shnf",
          "venue": "Barton Hall",
          "transferer": "Rob Eaton",
          "downloads": 80431,
          "avg_rating": 4.5,
          "num_reviews": 13,
          "source": "Soundboard",
          "lineage": "lineage"
        }
      ],
      "count": 1,
      "cursor": "cursor",
      "total": 2
    }
  JSON

  SECOND_JSON = <<~JSON.freeze
    {
      "items": [
        {
          "date": "1978-05-08T00:00:00Z",
          "coverage": "North Pole, SD",
          "identifier": "gd78-05-08.sbd.hicks.4982.sbeok.shnf",
          "venue": "Candy Land",
          "transferer": "Rob Eaton",
          "downloads": 80431,
          "avg_rating": 4.5,
          "num_reviews": 13,
          "source": "Matrix",
          "lineage": "lineage"
        }
      ],
      "count": 2,
      "total": 2
    }
  JSON

  TRACKS_JSON = <<~JSON.freeze
    {
      "result": [
        {
          "name": "gd77-05-08eaton-d3t07.mp3",
          "source": "derivative",
          "creator": "Grateful Dead",
          "track": "02",
          "album": "1978-05-08 - Barton Hall, Cornell University",
          "bitrate": "185",
          "length": "06:10",
          "format": "VBR MP3",
          "original": "gd77-05-08eaton-d3t07.shn",
          "mtime": "1417376301",
          "size": "8556544",
          "md5": "c665f7fc8e57958e8601cf4cf314e299",
          "crc32": "b2de6a02",
          "sha1": "8ab421210babd02a77957e6854690596c82a3b85",
          "height": "0",
          "width": "0"
        },
        {
          "name": "gd77-05-08eaton-d3t07.flac",
          "source": "derivative",
          "creator": "Grateful Dead",
          "album": "1978-05-08 - Barton Hall, Cornell University",
          "bitrate": "185",
          "length": "370.00",
          "format": "Flac",
          "original": "gd77-05-08eaton-d3t07.shn",
          "mtime": "1417376301",
          "size": "8556544",
          "md5": "c665f7fc8e57958e8601cf4cf314e299",
          "crc32": "b2de6a02",
          "sha1": "8ab421210babd02a77957e6854690596c82a3b85",
          "height": "0",
          "width": "0"
        },
        {
          "name": "gd77-05-08eaton-d3t07.ogg",
          "source": "derivative",
          "format": "Ogg Vorbis",
          "original": "gd77-05-08eaton-d3t07.shn",
          "mtime": "1417376570",
          "size": "4122537",
          "md5": "d40bbded1fe711e79a8e244e5a603b8e",
          "crc32": "31ce8f31",
          "sha1": "e4927ea269847f429c9e053cd5954c862ff54700",
          "length": "370.74",
          "height": "0",
          "width": "0"
        },
        {
          "name": "gd77-05-08eaton-d3t07.shn",
          "source": "original",
          "format": "Shorten",
          "md5": "b921edcbd47b43698d892b3d1cfc640b",
          "creator": "Grateful Dead",
          "track": "02",
          "album": "1977-05-08 - Barton Hall, Cornell University",
          "title": "Saturday Night",
          "mtime": "1059317108",
          "size": "41463445",
          "crc32": "58f3091b",
          "sha1": "94bce2685ca213f70149f520f929fa95cbd3fa71",
          "length": "235.05",
          "private": "true",
          "height": "0",
          "width": "0"
        }
      ]
    }
  JSON
end
