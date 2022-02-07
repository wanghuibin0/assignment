fun is_older (a : int * int * int, b : int * int * int) =
    let val ta = #1 a * 366 + #2 a * 31 + #3 a
        val tb = #1 b * 366 + #2 b * 31 + #3 b
    in
        ta < tb
    end
(* val x0 = is_older((1990,4,17), (2022,2,7)) *)
(* val y0 = is_older((1990,4,17), (1990,4,17)) *)
(* val z0 = is_older((2022,2,8), (2022,2,7)) *)

fun number_in_month (a : (int * int * int) list, b : int) =
    let fun date_in_a_month (x : (int * int * int), y : int) =
            #2 x = y
    in
        if null a
        then 0
        else if date_in_a_month(hd a, b)
        then 1+number_in_month(tl a, b)
        else
            number_in_month(tl a, b)
    end
(* val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1 *)


fun number_in_months (a : (int * int * int) list, b : int list) =
    if null b
    then 0
    else
        number_in_month(a, hd b) + number_in_months(a, tl b)

(* val x1 = number_in_months([], []) (* 0 *) *)
(* val y1 = number_in_months([(123, 3, 12)], [2, 3, 9]) (* 1 *) *)
(* val z1 = number_in_months([(2012, 4, 18)], [12]) (* 0 *) *)
(* val a1 = number_in_months([(2012, 4, 18), (123, 12, 12), (123, 12, 30)], [12]) (* 2 *) *)

fun dates_in_month (a : (int * int * int) list, b : int) =
    if null a
    then []
    else
        let fun date_in_month (a : (int * int * int), b : int) =
                #2 a = b
        in
            if date_in_month(hd a, b)
            then hd a :: dates_in_month(tl a, b)
            else dates_in_month(tl a, b)
        end

(* val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)
fun dates_in_months (a : (int * int * int) list, b : int list) =
    if null b
    then []
    else
        let fun append (x, y) =
                if null x
                then y
                else
                    hd x :: append(tl x, y)
        in
            append(dates_in_month(a, hd b), dates_in_months(a, tl b))
        end

(* val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)

fun get_nth (a, n : int) =
    if n = 1
    then hd a
    else
        get_nth(tl a, n-1)

(* val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)

fun date_to_string (a : int * int * int) =
    let val months = ["January",
                      "February",
                      "March",
                      "April",
                      "May",
                      "June",
                      "July",
                      "August",
                      "September",
                      "October",
                      "November",
                      "December"]
        val month = get_nth(months, #2 a)
    in
        month ^ " " ^ Int.toString(#3 a) ^ ", " ^ Int.toString(#1 a)
    end

(* val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)

fun number_before_reaching_sum (sum : int, a : int list) =
    if hd a >= sum
    then 0
    else
        1 + number_before_reaching_sum(sum - hd a, tl a)

(* val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)

fun what_month (a : int) =
    let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1+number_before_reaching_sum(a, days_in_month)
    end
(* val test9 = what_month 70 = 3 *)

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
        what_month(day1) :: month_range(day1+1, day2)

(* val test10 = month_range (31, 34) = [1,2,2,2] *)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let val oldest_rest = oldest(tl dates)
        in
            if not (isSome oldest_rest)
            then SOME (hd dates)
            else if is_older(hd dates, valOf oldest_rest)
            then SOME (hd dates)
            else oldest_rest
        end

(* val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)

fun removeDup(x : int list) =
    if null x
    then []
    else
        let fun exsits (y : int, z : int list) =
                if null z
                then false
                else if hd z = y
                then true
                else exsits(y, tl z)
            val restRemove = removeDup(tl x)
        in
            if exsits(hd x, restRemove)
            then restRemove
            else
                hd x :: restRemove
        end
fun number_in_months_challenge (a : (int * int * int) list, b : int list) =
    number_in_months(a, removeDup(b))

(* val test12 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = 3 *)

fun dates_in_months_challenge (a : (int * int * int) list, b : int list) =
    dates_in_months(a, removeDup(b))
(* val test120 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)

fun reasonable_date (date : int * int * int) =
    let fun is_leap_year (year : int) =
            year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val days =
            if is_leap_year(#1 date)
            then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        #1 date > 0 andalso #2 date >= 1 andalso #2 date <= 12 andalso #3 date >= 1 andalso #3 date <= get_nth(days, #2 date)
    end
        (* val test13 = reasonable_date(2022, 2, 29) = false *)
