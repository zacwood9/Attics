module ApplicationHelper
  def authenticated?
    Current.user.present?
  end
end
