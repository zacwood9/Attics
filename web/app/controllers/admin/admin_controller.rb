class Admin::AdminController < ActionController::Base
  before_action :authenticate

  private

  def authenticate
    authenticate_or_request_with_http_basic do |username, password|
      target_username = Rails.application.credentials.dig(:admin, :username)
      target_password = Rails.application.credentials.dig(:admin, :password)

      ActiveSupport::SecurityUtils.secure_compare(username, target_username) &&
        ActiveSupport::SecurityUtils.secure_compare(password, target_password)
    end
  end
end
