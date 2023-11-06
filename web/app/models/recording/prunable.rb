module Recording::Prunable
  def prune
    Recording::Pruner.new(self).prune
  end
end
