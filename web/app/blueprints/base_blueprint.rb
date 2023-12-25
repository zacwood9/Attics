class BaseBlueprint < Blueprinter::Base
  def self.field_with_default(field_key, default)
    raise ArgumentError.new("must provide default") if default.nil?

    field field_key do |object|
      object.send(field_key) || default
    end
  end
end
