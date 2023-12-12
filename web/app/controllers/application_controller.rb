class ApplicationController < ActionController::Base
  before_action :set_current_request_details
  before_action :authenticate

  private
    def authenticate
      if (session_record = Session.find_by_id(cookies.signed[:session_token]))
        Current.session = session_record
      end
    end

    def set_current_request_details
      Current.user_agent = request.user_agent
      Current.ip_address = request.ip
    end

    def require_lock(wait: 1.hour, attempts: 10)
      # counter = Kredis.counter("require_lock:#{request.remote_ip}:#{controller_path}:#{action_name}", expires_in: wait)
      # counter.increment
      #
      # if counter.value > attempts
      #   redirect_to root_path, alert: "You've exceeded the maximum number of attempts"
      # end
    end
end
