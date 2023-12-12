class AuthenticatedController < ApplicationController
  before_action :redirect_unless_authenticated

  private

  def redirect_unless_authenticated
    if Current.user.nil?
      redirect_to sign_in_path
    end
  end
end
