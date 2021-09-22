compute.bundestag = function() {
  library(restorepoint)
  library(dplyr)
  library(readr)

  dat = readr::read_csv("prognose_2021.csv")
  #row = which(dat$land == "Bayern" & dat$party == "Union")
  #dat$party[row] = "CSU"

  res = compute.seats(dat)
  summarize.results(res)

  total.seats = 598
  df = runde1(dat, total.seats = 598)

  ov = oberverteilung2(df, max.ueberhang = 3)
  uv = unterverteilung2(df, ov)
  uv

}

summarize.results = function(res) {
  cat("Total size: ", sum(res$seats), " seats\n\n")
  res %>%
    mutate(total_votes = sum(votes),
           total_seats = sum(seats)) %>%
    group_by(party) %>%
    summarize(
      vote_share = sum(votes) / first(total_votes),
      seat_shares = sum(seats) / first(total_seats),
      seats = sum(seats),
      ueberhang = first(ueberhang_party)
    )

}

compute.seats = function(dat, max.ueberhang = 3) {
  options(dplyr.summarise.inform = FALSE)

  df = runde1(dat, total.seats = 598)

  ov = oberverteilung2(df, max.ueberhang = max.ueberhang)
  uv = unterverteilung2(df, ov) %>%
    mutate(ueberhang_party = seats2_party-seats2_sls_party)
  select(uv, land, pop, votes, direct, seats=seats2, seats.mr=seats.mr, ueberhang_party)
}


runde1 = function(dat, total.seats = 598) {

  land.df = dat %>%
    group_by(land) %>%
    summarize(pop = first(pop)) %>%
    ungroup() %>%
    mutate(land_seats = sls(pop,total.seats))

  df = dat %>% left_join(land.df,by = c("land", "pop"))

  df = df %>%
    group_by(land) %>%
    mutate(seats1_sls = sls(votes, first(land_seats))) %>%
    mutate(drohender_ueberhang = pmax(direct-seats1_sls,0)) %>%
    mutate(min_seats = pmax(direct, ceiling(0.5*(direct+seats1_sls))))

  return(df)

}


oberverteilung2 = function(df, total.seats = 598, max.ueberhang=3, max.size = 5000, verbose=FALSE) {
  restore.point("compute.oberverteilung2")
  party.df = df %>%
    group_by(party) %>%
    summarize(
      drohend_ueberhang_party = sum(drohender_ueberhang),
      direct_party = sum(direct),
      votes_party = sum(votes),
      seats_min_party = sum(min_seats)
    )

  start.seats = max(sum(party.df$seats_min_party), total.seats)

  ts = 700
  ts = 833
  if (verbose) cat("Oberverteilung: ")
  for (ts in start.seats:max.size) {
    if (verbose) cat("\n",ts)
    party.df = party.df %>% mutate(
      seats2_sls_party = sls(votes_party, ts),
      ueberhang2_party = pmax(seats_min_party-seats2_sls_party,0),
      seats2_party = pmax(seats_min_party, seats2_sls_party)
    )
    if (verbose) cat(" Ueberhang = ", sum(party.df$ueberhang2_party))
    ok = sum(party.df$ueberhang2_party) <= max.ueberhang
    if (ok) {
      return(party.df)
    }
  }
}

unterverteilung2 = function(df, ov.df) {
  restore.point("unterverteilung2")

  uv.df = left_join(df, ov.df, by="party")

  uv.df = uv.df %>%
    group_by(party) %>%
    mutate(seats2 = sls(votes=votes, total.seats=first(seats2_party),min.seats = min_seats))
  uv.df
}



# Sainte-Laguë/Schepers extend by minimum seats
sls = function(votes, total.seats = 299, parties = names(votes), min.seats=NULL, details=FALSE) {
  restore.point("saint.lague.schepers")
  sv = sum(votes)
  names(votes) = parties

  # Search for a divisor that works
  if (is.null(min.seats)) {
    f = function(divisor) {
      sum(round(votes/divisor)) - total.seats
    }
  } else {
    f = function(divisor) {
      sum(pmax(round(votes/divisor), min.seats)) - total.seats
    }
  }

  # check if it works with standard divisor
  div = sv / total.seats

  # If default divisior does not work: check different divisors
  if (f(div) != 0) {
    # Add partial votes to avoid ties
    votes = votes + runif(length(votes), 0, 0.01)
    # Search for a uni root
    res = uniroot(f, c(0.25*div, div*3),extendInt = "no")
    div = res$root
  }

  if (is.null(min.seats)) {
    seats =  round(votes/div)
  } else {
    seats = pmax(round(votes/div), min.seats)
  }
  if (!details) return(seats)
  return(list(seats=seats, div=div, sum.seats=sum(seats), min.seats=min.seats))
}

