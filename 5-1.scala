class Time(val hours :Int, val minutes:Int) {
  def before(other: Time) = { hours < other.hours || (hours == other.hours && minutes < other.minutes) }
}

class Time(val hrs: Int, val min: Int) {
	def before(other: Time) = {
		(hrs < other.hrs) || (hrs == other.hrs && min < other.min)
	}
}
