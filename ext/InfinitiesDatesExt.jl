module InfinitiesDatesExt

using Infinities: @archimedean, @archimedean_magnitude
using Dates: Year, Quarter, Month, Week, Day, Hour, Minute, Second, Millisecond, Microsecond,
             Nanosecond, Date, DateTime

# `Time` is left out, as it models a bounded quantity which wraps at midnight.
# Calendar periods are not comparable with fixed-duration periods.
@archimedean_magnitude Year Quarter Month Week Day Hour Minute Second Millisecond Microsecond Nanosecond
@archimedean Date DateTime

end
