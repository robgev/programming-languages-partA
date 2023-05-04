fun is_older (date1: (int * int * int), date2: (int * int *int)) =
  if (#1 date1 = #1 date2)
  then 
    if (#2 date1 = #2 date2) 
    then
      #3 date1 < #3date2
    else #2 date1 < #2 date2
  else #1 date1 < #1 date2 

fun number_in_month (dates: (int * int * int) list, month: int) = 
  if null dates
  then 0
  else
    (if #2 (hd dates) = month then 1 else 0) + number_in_month(tl dates, month)

fun number_in_months (dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else
    number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates: (int * int * int) list, month: int) =
  if null dates
  then []
  else 
    let
      val dates_in_month_tl = dates_in_month(tl dates, month)
    in
      if #2 (hd dates) = month 
      then 
      hd dates :: dates_in_month_tl
      else 
      dates_in_month_tl
    end

fun dates_in_months (dates: (int * int * int) list, months: int list) =
  if null months
  then []
  else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strings: string list, n: int) = 
  if n = 1
  then
    hd strings
  else 
    get_nth(tl strings, n - 1)

fun date_to_string (date: (int * int * int)) = 
  let 
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in 
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum: int, nums: int list) = 
  if sum <= hd nums
  then 0
  else 
    1 + number_before_reaching_sum(sum - hd nums, tl nums)

fun what_month (day: int) = 
  let 
    val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_months) + 1
  end

fun month_range (day1: int, day2: int) = 
  if day1 > day2
  then []
  else 
    what_month day1 :: month_range(day1 + 1, day2)

fun oldest (dates: (int * int * int) list) = 
  if null dates
  then NONE 
  else
    let 
      fun oldest_nonempty (dates: (int * int * int) list) =
        if null (tl dates) 
        then hd dates
        else 
          let val oldest_in_tail = oldest_nonempty(tl dates)
          in 
            if (is_older(hd dates, oldest_in_tail))
            then hd dates
            else oldest_in_tail
          end
    in
      SOME (oldest_nonempty dates)
    end

fun remove_dups (xs: int list) = 
  let 
    fun is_in (items: int list, new_item: int) =
      if null items
      then false
      else
        hd items = new_item orelse is_in(tl items, new_item)
  in
    if null xs
    then []
    else
      let 
        val remove_dups_tail = remove_dups(tl xs)
      in
        if not (is_in(remove_dups_tail, hd xs))
        then hd xs :: remove_dups_tail
        else remove_dups_tail
      end
  end

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) = 
  number_in_months(dates, remove_dups(months))

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) = 
  dates_in_months(dates, remove_dups(months))

fun reasonable_date (date: (int * int * int)) =
  let 
    fun get_number_of_days () = 
      let 
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val is_leap = (#1 date mod 400 = 0) orelse ((#1 date mod 4 = 0) andalso (#1 date mod 100 <> 0))
        fun get_nth (months: int list, n: int) = 
          if n = 1
          then
            hd months
          else 
            get_nth(tl months, n - 1)
      in
        if is_leap andalso (#2 date = 2)
        then 29
        else
          get_nth(days_in_months, #2 date)
      end
  in
    (#1 date > 0) andalso (#2 date >= 1) andalso (#2 date <= 12) andalso (#3 date >= 1) andalso (#3 date <= get_number_of_days())
  end