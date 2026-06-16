library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

bundled_workbook <- file.path("data", "Last War Price Guide.xlsx")
app_build_label <- "Build: 2026-06-07 weekly source network weights"
icon_cache_bust <- "20260604a"
source_workbook <- if (file.exists(bundled_workbook)) {
  bundled_workbook
} else {
  file.path(Sys.getenv("USERPROFILE"), "Desktop", "Last War Price Guide.xlsx")
}
season_choices <- c("Season 1", "Preseason")

parse_num <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9.\\-]", "")
  out <- suppressWarnings(as.numeric(x))
  out[is.na(x) | x == ""] <- NA_real_
  out
}

clean_item_key <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  x <- str_replace_all(x, "décor", "decor")
  x <- str_replace_all(x, "®", "")
  x <- str_replace_all(x, "skil chip", "skill chip")
  x <- str_replace_all(x, "dialectric|dialetric", "dielectric")
  x <- str_replace_all(x, "\\bdrone part\\b", "drone parts")
  x <- str_replace_all(x, "\\blevel\\b", "lv")
  x <- str_replace_all(x, "\\b1 hour\\b", "1h")
  x <- str_replace_all(x, "\\bhour\\b", "h")
  x <- str_replace_all(x, "universal ur hero shard|ur hero universal shard|universal ur hero universal shard", "ur hero universal shard")
  x <- str_replace_all(x, "ssr resource choice chest|resource choice chest \\(ssr\\)", "resource choice chest ssr")
  x <- str_replace_all(x, "ur resource choice chest|resource choice chest \\(ur\\)", "resource choice chest ur")
  x <- str_replace_all(x, "research choice chest \\(ur\\)", "research choice chest ur")
  x <- str_replace_all(x, "\\((sr|ssr|ur|mr|10k)\\)", "\\1")
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

pretty_item_name <- function(key) {
  small <- c("ur", "ssr", "sr", "mr", "lv", "h", "m", "exp")
  words <- str_split(key, " ", simplify = FALSE)
  vapply(words, function(w) {
    paste(vapply(w, function(part) {
      if (part %in% small) toupper(part) else str_to_title(part)
    }, character(1)), collapse = " ")
  }, character(1))
}

item_menu_label <- function(x) {
  x %>%
    str_replace_all("^Universal Decor Component Equivalent$", "Decoration Chest/Components") %>%
    str_replace_all("\\s*\\([^)]*Equivalent\\)", "") %>%
    str_replace_all("\\s+Equivalent\\b", "") %>%
    str_squish()
}

standardize_display_item <- function(item) {
  if (is.na(item) || item == "") return(item)
  key <- clean_item_key(item)
  tier <- detect_tier(key)
  hours <- duration_hours(item)

  if (key == "survivor s token") {
    return("Survivor's Token")
  }

  if (str_detect(key, "drone combat boost")) {
    return("Drone Combat Boost EXP")
  }

  speed_type <- case_when(
    str_detect(key, "construction speed up") ~ "Construction",
    str_detect(key, "research speed up") ~ "Research",
    str_detect(key, "training speed up") ~ "Training",
    str_detect(key, "healing speed up") ~ "Healing",
    str_detect(key, "universal speed up|\\bspeed up\\b") ~ "Universal",
    TRUE ~ NA_character_
  )

  if (!is.na(speed_type) && !is.na(hours)) {
    duration_label <- if (hours < 1) {
      paste0(fmt_num(hours * 60, 0), "m")
    } else {
      paste0(fmt_num(hours, 0), "h")
    }
    return(paste(duration_label, speed_type, "Speed-Up"))
  }

  if (str_detect(key, "hero exp chest") && !is.na(tier)) {
    return(paste0("Hero EXP Chest (", toupper(tier), ")"))
  }

  if (str_detect(key, "resource choice chest") && !is.na(tier)) {
    return(paste0("Resource Choice Chest (", toupper(tier), ")"))
  }

  component_level <- str_match(key, "\\blv\\.?\\s*([0-9]+)\\s*(?:drone\\s*)?component")[, 2]
  if (!is.na(component_level)) {
    if (str_detect(key, "choice chest")) {
      return(paste0("Lv.", component_level, " Drone Component Choice Chest"))
    }
    return(paste0("Lv.", component_level, " Drone Component Chest"))
  }

  if (key == "drone part" || key == "drone parts") {
    return("Drone Parts")
  }

  if (str_detect(key, "dielectric ceramic")) {
    return("Dielectric Ceramic")
  }

  if (key %in% c("universal ur hero shard", "ur hero universal shard")) {
    return("UR Hero Universal Shard")
  }

  item
}

duration_hours <- function(item) {
  item_text <- str_to_lower(str_squish(as.character(item)))
  duration <- str_match(item_text, "\\b([0-9]+(?:\\.[0-9]+)?)\\s*-?\\s*(m|min|minute|minutes|h|hr|hour|hours)\\b")
  amount <- suppressWarnings(as.numeric(duration[, 2]))
  unit <- duration[, 3]

  case_when(
    unit %in% c("m", "min", "minute", "minutes") ~ amount / 60,
    unit %in% c("h", "hr", "hour", "hours") ~ amount,
    TRUE ~ NA_real_
  )
}

resource_chest_amounts <- function(hq_level = 29) {
  # Source: Cpt Hedge resource chest calculator, credited there to kp7 from #163.
  # The calculator exposes common/rare/epic/legendary values; this app maps those
  # to R/SR/SSR/UR and uses SR as the base unit.
  sr_values <- tibble::tribble(
    ~hq_level, ~food, ~iron, ~coins, ~hero_exp,
    20, 115000, 115000, 68750, 339840,
    21, 118800, 118800, 71280, 374700,
    22, 126000, 126000, 75600, 413100,
    23, 129600, 129600, 77760, 455460,
    24, 136800, 136800, 82000, 501250,
    25, 140400, 140400, 84240, 553560,
    26, 147600, 147600, 88560, 610320,
    27, 154800, 154800, 92880, 672900,
    28, 158400, 158400, 95040, 741840,
    29, 165600, 165600, 99300, 817860,
    30, 169200, 169200, 101520, 901740
  )

  sr_values %>%
    pivot_longer(-hq_level, names_to = "resource", values_to = "sr_amount") %>%
    tidyr::crossing(tier = c("r", "sr", "ssr", "ur")) %>%
    mutate(amount = sr_amount * resource_tier_multiplier(tier)) %>%
    select(hq_level, resource, tier, amount) %>%
    filter(hq_level == !!hq_level)
}

detect_tier <- function(key) {
  case_when(
    str_detect(key, "\\bur\\b") ~ "ur",
    str_detect(key, "\\bssr\\b") ~ "ssr",
    str_detect(key, "\\bsr\\b") ~ "sr",
    str_detect(key, "\\br\\b") ~ "r",
    TRUE ~ NA_character_
  )
}

resource_tier_multiplier <- function(tier) {
  case_when(
    tier == "r" ~ 0.1,
    tier == "sr" ~ 1,
    tier == "ssr" ~ 8,
    tier == "ur" ~ 24,
    TRUE ~ NA_real_
  )
}

normalize_one_listing <- function(row, hq_level = 29) {
  item <- row$item
  qty <- row$qty
  key <- clean_item_key(item)
  hours <- duration_hours(item)

  speed_type <- case_when(
    str_detect(key, "construction speed up") ~ "construction",
    str_detect(key, "research speed up") ~ "research",
    str_detect(key, "training speed up") ~ "training",
    str_detect(key, "healing speed up") ~ "healing",
    str_detect(key, "universal speed up") ~ "universal",
    str_detect(key, "\\bspeed up\\b") ~ "universal",
    TRUE ~ NA_character_
  )

  if (!is.na(speed_type) && !is.na(hours)) {
    return(tibble(
      item_key = paste(speed_type, "speed up hour"),
      item_canonical = paste0(str_to_title(speed_type), " Speed-Up (1h Equivalent)"),
        comparable_qty = qty * hours,
        comparable_unit = "1h",
        flexibility_rank = if_else(speed_type == "universal", 0L, 1L),
        anchor_group = item_key,
        normalization_note = paste0("Normalized from ", item, " to 1 hour.")
    ))
  }

  if (str_detect(key, "\\bshield\\b") && !is.na(hours)) {
    return(tibble(
      item_key = "shield 8 hour equivalent",
      item_canonical = "Shield (8-Hour Equivalent)",
      comparable_qty = qty * hours / 8,
      comparable_unit = "8-Hour Shield",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = paste0("Normalized from ", item, " to 8-hour shields.")
    ))
  }

  if (str_detect(key, "battle data 10k")) {
    return(tibble(
      item_key = "battle data",
      item_canonical = "Battle Data (10k Equivalent)",
      comparable_qty = 1,
      comparable_unit = "10k Battle Data",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Battle Data (10K) is the base denomination."
    ))
  }

  if (str_detect(key, "battle data 100k")) {
    return(tibble(
      item_key = "battle data",
      item_canonical = "Battle Data (10k Equivalent)",
      comparable_qty = 10,
      comparable_unit = "10k Battle Data",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Battle Data (100K) is treated as 10 10K Battle Data units."
    ))
  }

  if (key == "battle data") {
    return(tibble(
      item_key = "battle data",
      item_canonical = "Battle Data (10k Equivalent)",
      comparable_qty = qty / 10000,
      comparable_unit = "10k Battle Data",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = ""
    ))
  }

  direct_resource <- case_when(
    key == "iron" ~ "iron",
    key == "food" ~ "food",
    key == "coins" ~ "coins",
    TRUE ~ NA_character_
  )

  if (!is.na(direct_resource)) {
    sr_amount <- resource_chest_amounts(hq_level) %>%
      filter(resource == direct_resource, tier == "sr") %>%
      pull(amount)
    if (!length(sr_amount)) {
      sr_amount <- resource_chest_amounts(29) %>%
        filter(resource == direct_resource, tier == "sr") %>%
        pull(amount)
    }

    return(tibble(
      item_key = paste(direct_resource, "resource"),
      item_canonical = paste0(str_to_title(direct_resource), " Resource"),
      comparable_qty = qty / sr_amount,
      comparable_unit = "SR Resource Chest",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = ""
    ))
  }

  chest_resource <- case_when(
    str_detect(key, "iron chest") ~ "iron",
    str_detect(key, "food chest") ~ "food",
    str_detect(key, "coin chest") ~ "coins",
    TRUE ~ NA_character_
  )
  is_resource_choice <- str_detect(key, "resource choice chest")
  tier <- detect_tier(key)

  if ((!is.na(chest_resource) || is_resource_choice) && !is.na(tier)) {
    resources <- if (is_resource_choice) c("iron", "food", "coins") else chest_resource
    chest_anchor_group <- if (is_resource_choice) paste("resource choice chest", tier) else key
    amounts <- resource_chest_amounts(hq_level) %>%
      filter(resource %in% resources, tier == !!tier)

    if (nrow(amounts) == 0) {
      amounts <- resource_chest_amounts(29) %>%
        filter(resource %in% resources, tier == !!tier)
      resource_note <- "HQ 29 chest content is used as a placeholder until this HQ level is entered."
    } else {
      resource_note <- paste0("Chest content uses HQ ", hq_level, " values.")
    }

    return(amounts %>%
      left_join(
        resource_chest_amounts(hq_level) %>%
          filter(tier == "sr") %>%
          select(resource, sr_amount = amount),
        by = "resource"
      ) %>%
      transmute(
        item_key = paste(resource, "resource"),
        item_canonical = paste0(str_to_title(resource), " Resource"),
        comparable_qty = qty * resource_tier_multiplier(tier),
        comparable_unit = "SR Resource Chest",
        flexibility_rank = if_else(is_resource_choice, 0L, 1L),
        anchor_group = chest_anchor_group,
        normalization_note = paste0(toupper(tier), " ", resource_note)
      ))
  }

  if (str_detect(key, "hero exp chest")) {
    hero_exp_multiplier <- case_when(
      tier == "ur" ~ 24,
      tier == "ssr" ~ 8,
      tier == "sr" ~ 1,
      TRUE ~ NA_real_
    )
    if (!is.na(hero_exp_multiplier)) {
      return(tibble(
        item_key = "hero exp chest sr equivalent",
        item_canonical = "Hero EXP Chest (SR Equivalent)",
        comparable_qty = qty * hero_exp_multiplier,
        comparable_unit = "SR Hero EXP Chest",
        flexibility_rank = 1L,
        anchor_group = item_key,
        normalization_note = "Hero EXP chest tiers use SSR = 8x SR and UR = 3x SSR."
      ))
    }
  }

  drone_level <- str_match(key, "\\blv\\s*([0-9]+)\\s*(?:drone\\s*)?component")
  if (!is.na(drone_level[, 2])) {
    level <- as.numeric(drone_level[, 2])
    is_component_choice <- str_detect(key, "choice chest")
    return(tibble(
      item_key = if_else(is_component_choice, "drone component choice level 1 equivalent", "drone component level 1 equivalent"),
      item_canonical = if_else(is_component_choice, "Drone Component Choice (Lv 1 Equivalent)", "Drone Component (Lv 1 Equivalent)"),
      comparable_qty = qty * (3 ^ (level - 1)),
      comparable_unit = if_else(is_component_choice, "Lv 1 Component Choices", "Lv 1 Components"),
      flexibility_rank = if_else(is_component_choice, 0L, 1L),
      anchor_group = item_key,
      normalization_note = if_else(
        is_component_choice,
        "Drone component choice chests use 3 lower-level components per next level, but fit as a flexible choice item.",
        "Drone components use 3 lower-level components per next level."
      )
    ))
  }

  material_power <- case_when(
    key == "superalloy" ~ 0,
    key == "synthetic resin" ~ 1,
    str_detect(key, "dielectric ceramic") ~ 2,
    TRUE ~ NA_real_
  )
  if (!is.na(material_power)) {
    return(tibble(
      item_key = "superalloy equivalent",
      item_canonical = "Superalloy / Resin / Ceramic Materials",
      comparable_qty = qty * (4 ^ material_power),
      comparable_unit = "superalloy",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Crafting-material chain uses 4 of each level for 1 of the next."
    ))
  }

  if (key == "universal decor component") {
    return(tibble(
      item_key = "universal decor component equivalent",
      item_canonical = "Universal Decor Component Equivalent",
      comparable_qty = qty,
      comparable_unit = "Universal Decor Component",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Universal decor components are the base decoration unit."
    ))
  }

  if (key == "decoration chest ur") {
    return(tibble(
      item_key = "universal decor component equivalent",
      item_canonical = "Universal Decor Component Equivalent",
      comparable_qty = qty * 130,
      comparable_unit = "Universal Decor Component",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "1 UR decoration chest is treated as 130 universal decor components."
    ))
  }

  if (key %in% c("hero choice chest", "ur hero universal shard")) {
    return(tibble(
      item_key = "ur hero shard equivalent",
      item_canonical = "UR Hero Shard Equivalent",
      comparable_qty = qty,
      comparable_unit = "shard",
      flexibility_rank = if_else(key == "hero choice chest", 0L, 1L),
      anchor_group = item_key,
      normalization_note = "Hero choice chests and UR universal shards are treated as equivalent for now."
    ))
  }

  if (key %in% c("sr hero universal shard", "ssr hero universal shard")) {
    shard_tier <- if_else(str_detect(key, "\\bssr\\b"), "SSR", "SR")
    return(tibble(
      item_key = paste0(str_to_lower(shard_tier), " hero shard equivalent"),
      item_canonical = paste0(shard_tier, " Hero Shard Equivalent"),
      comparable_qty = qty,
      comparable_unit = "shard",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = paste0(shard_tier, " hero universal shards are treated as the base ", shard_tier, " shard item.")
    ))
  }

  if (str_detect(key, "skill chip chest") && !is.na(tier)) {
    skill_chip_ev <- case_when(
      tier == "r" ~ "EV: 65% R, 30% SR, 4.8% SSR, 0.2% UR + 1 material",
      tier == "sr" ~ "EV: 68% SR, 30% SSR, 2% UR + 5 material",
      tier == "ssr" ~ "EV: 85% SSR, 15% UR + 30 material",
      tier == "ur" ~ "100% UR + 100 material",
      TRUE ~ paste0(toupper(tier), " Skill Chip Chest")
    )
    return(tibble(
      item_key = paste("skill chip chest", tier),
      item_canonical = paste0("Skill Chip Chest (", toupper(tier), ")"),
      comparable_qty = qty,
      comparable_unit = skill_chip_ev,
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Skill chip chest baseline uses expected chip/material contents."
    ))
  }

  if (str_detect(key, "skill chip chest") && is.na(tier)) {
    return(tibble(
      item_key = "skill chip chest r",
      item_canonical = "Skill Chip Chest (R)",
      comparable_qty = qty,
      comparable_unit = "EV: 65% R, 30% SR, 4.8% SSR, 0.2% UR + 1 material",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Untiered skill chip chest label is treated as R."
    ))
  }

  if (key == "survivor s token") {
    return(tibble(
      item_key = key,
      item_canonical = "Survivor's Token",
      comparable_qty = qty,
      comparable_unit = "unit",
      flexibility_rank = 1L,
      anchor_group = key,
      normalization_note = ""
    ))
  }

  tibble(
    item_key = key,
    item_canonical = pretty_item_name(key),
    comparable_qty = qty,
    comparable_unit = "unit",
    flexibility_rank = 1L,
    anchor_group = key,
    normalization_note = ""
  )
}

filter_prices_for_season <- function(prices, season = "Season 1") {
  season <- ifelse(is.null(season) || is.na(season) || season == "", "Season 1", season)
  if (season == "Preseason") {
    prices <- prices %>%
      filter(store != "Season Storefront") %>%
      filter(store != "Serum Shop") %>%
      filter(!(store == "Honor Storefront" & item == "Universal Exclusive Weapon Shard"))
  }
  prices
}

season_extra_listings <- function(season = "Season 1") {
  season <- ifelse(is.null(season) || is.na(season) || season == "", "Season 1", season)
  rows <- tibble(
    item = character(),
    qty = numeric(),
    price = numeric(),
    curr = character(),
    limit = numeric(),
    store = character()
  )

  if (season == "Season 1") {
    rows <- bind_rows(rows, tibble::tribble(
      ~item, ~qty, ~price, ~curr, ~limit, ~store,
      "Mason Shard", 1, 1000, "ALL", 30, "Alliance Storefront",
      "Universal Exclusive Weapon Shard", 1, 300, "CAM", 10, "Campaign Storefront",
      "Universal Exclusive Weapon Shard", 1, 120, "COUR", 10, "Zombie Invasion Store",
      "Skill Chip Chest (SR)", 1, 120, "COUR", 3, "Zombie Invasion Store"
    ))
  }

  if (season %in% c("Season 1", "Preseason")) {
    rows <- bind_rows(rows, tibble::tribble(
      ~item, ~qty, ~price, ~curr, ~limit, ~store,
      "Stamina", 50, 2000, "ALL", 5, "Alliance Storefront"
    ))
  }

  if (!nrow(rows)) {
    return(tibble(
      item = character(),
      qty = numeric(),
      price = numeric(),
      curr = character(),
      limit = numeric(),
      store = character()
    ))
  }
  rows
}

load_prices <- function(path = source_workbook, hq_level = 29, season = "Season 1") {
  raw <- read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  starts <- which(!is.na(as.character(raw[1, ])) & as.character(raw[2, ]) == "Item")

  rows <- lapply(starts, function(st) {
    block <- raw[3:nrow(raw), st:(st + 4)]
    names(block) <- c("item", "qty", "price", "curr", "limit")
    block %>%
      mutate(
        store = str_replace(as.character(raw[[st]][1]), "Invation", "Invasion"),
        item = as.character(item),
        item = if_else(str_detect(clean_item_key(item), "skill chip chest") & !str_detect(clean_item_key(item), "\\bsr\\b|\\bssr\\b|\\bur\\b"),
                       "Skill Chip Chest (R)", item),
        item = case_when(
          str_detect(clean_item_key(item), "battle data 10k") ~ "Battle Data (10k)",
          clean_item_key(item) == "battle data" & parse_num(qty) == 100000 ~ "Battle Data (100K)",
          clean_item_key(item) == "battle data" & parse_num(qty) == 10000 ~ "Battle Data (10k)",
          str_replace(as.character(raw[[st]][1]), "Invation", "Invasion") == "Campaign Storefront" &
            str_detect(clean_item_key(item), "hero exp chest") ~ "Hero EXP Chest (SSR)",
          TRUE ~ item
        ),
        item = vapply(item, standardize_display_item, character(1)),
        qty = parse_num(qty),
        qty = case_when(
          clean_item_key(item) == "battle data 100k" & qty == 100000 ~ 1,
          clean_item_key(item) == "battle data 10k" & qty %in% c(10, 10000) ~ 1,
          TRUE ~ qty
        ),
        qty = if_else(str_replace(as.character(raw[[st]][1]), "Invation", "Invasion") == "Alliance Storefront" &
                        str_detect(clean_item_key(item), "\\bshield\\b"),
                      1, qty),
        price = parse_num(price),
        curr = str_to_upper(str_squish(as.character(curr))),
        limit = parse_num(limit)
      ) %>%
      filter(!is.na(item), item != "")
  })

  parsed <- bind_rows(rows, list(season_extra_listings(season))) %>%
    mutate(
      row_id = row_number()
    )

  normalized <- bind_rows(lapply(seq_len(nrow(parsed)), function(i) {
    bind_cols(parsed[i, ], normalize_one_listing(parsed[i, ], hq_level = hq_level))
  }))

  normalized %>%
    filter_prices_for_season(season) %>%
    mutate(unit_native = price / comparable_qty) %>%
    select(row_id, store, item, item_key, item_canonical, qty, comparable_qty,
           comparable_unit, flexibility_rank, anchor_group, normalization_note, price, curr, limit, unit_native)
}

build_value_model <- function(prices, efficient_percentile = 0.80) {
  include_in_network <- function(x) {
    !(x$store == "Bounty Hunter Trade Store" &
        clean_item_key(x$item) == "drone parts" &
        x$curr == "BOUN" &
        !is.na(x$price) &
        x$price == 32) &
      (
        clean_item_key(x$item) != "battle data 10k" |
          (
            x$store == "Campaign Storefront" &
              x$curr == "CAM" &
              !is.na(x$price) &
              x$price == 2000
          )
      )
  }

  with_network_keys <- function(x) {
    x %>%
      mutate(
        network_item_key = case_when(
          item_key %in% c("construction speed up hour", "research speed up hour", "training speed up hour", "healing speed up hour") ~ "specific speed up hour",
          item_key %in% c("iron resource", "food resource", "coins resource") ~ "resource chest sr equivalent",
          TRUE ~ item_key
        )
      )
  }

  network_rows <- prices %>%
    with_network_keys() %>%
    filter(store != "Wandering Merchant", include_in_network(.), !is.na(unit_native), unit_native > 0, !is.na(item_key), item_key != "", !is.na(curr), curr != "") %>%
    mutate(
      expansion_weight = 1 / ave(row_id, row_id, FUN = length),
      store_cadence_multiplier = if_else(
        store %in% c(
          "VIP Storefront",
          "Alliance Storefront",
          "Campaign Storefront",
          "ID Points Mall",
          "Luxury Choice Chest",
          "Deluxe Choice Chest"
        ),
        4,
        1
      ),
      finite_limit = if_else(is.na(limit), NA_real_, limit),
      listed_base_volume = comparable_qty * finite_limit * store_cadence_multiplier
    ) %>%
    group_by(network_item_key) %>%
    mutate(
      fallback_base_volume = median(listed_base_volume[!is.na(listed_base_volume) & listed_base_volume > 0], na.rm = TRUE),
      fallback_base_volume = if_else(is.finite(fallback_base_volume), fallback_base_volume, comparable_qty),
      listed_base_volume = if_else(is.na(listed_base_volume) | listed_base_volume <= 0, fallback_base_volume, listed_base_volume),
      item_median_base_volume = median(listed_base_volume[listed_base_volume > 0], na.rm = TRUE),
      item_median_base_volume = if_else(is.finite(item_median_base_volume) & item_median_base_volume > 0, item_median_base_volume, listed_base_volume),
      volume_weight = sqrt(listed_base_volume / item_median_base_volume),
      volume_weight = pmin(pmax(volume_weight, 0.35), 3)
    ) %>%
    ungroup() %>%
    mutate(
      base_weight = if_else(curr == "DIA", 1.5, 1) * expansion_weight * volume_weight
    )

  item_levels <- sort(unique(network_rows$network_item_key))
  curr_levels <- sort(setdiff(unique(network_rows$curr), "DIA"))
  n_items <- length(item_levels)
  n_currs <- length(curr_levels)
  x <- matrix(0, nrow = nrow(network_rows), ncol = n_items + n_currs)
  colnames(x) <- c(paste0("item::", item_levels), paste0("curr::", curr_levels))

  item_idx <- match(network_rows$network_item_key, item_levels)
  x[cbind(seq_len(nrow(network_rows)), item_idx)] <- 1
  curr_idx <- match(network_rows$curr, curr_levels)
  has_curr <- !is.na(curr_idx)
  x[cbind(which(has_curr), n_items + curr_idx[has_curr])] <- -1

  y <- log(network_rows$unit_native)
  weights <- network_rows$base_weight
  fit <- NULL
  fitted <- rep(NA_real_, nrow(network_rows))
  resid <- rep(NA_real_, nrow(network_rows))

  for (iter in seq_len(8)) {
    fit <- lm.wfit(x, y, w = weights)
    coef <- fit$coefficients
    coef[is.na(coef)] <- 0
    fitted <- as.vector(x %*% coef)
    resid <- y - fitted
    scale <- median(abs(resid - median(resid, na.rm = TRUE)), na.rm = TRUE) / 0.6745
    if (!is.finite(scale) || scale <= 0) scale <- sd(resid, na.rm = TRUE)
    if (!is.finite(scale) || scale <= 0) scale <- 1
    robust_weight <- pmin(1, (1.5 * scale) / pmax(abs(resid), 1e-9))
    weights <- network_rows$base_weight * robust_weight
  }

  coef <- fit$coefficients
  coef[is.na(coef)] <- 0
  item_values <- tibble(
    network_item_key = item_levels,
    direct_dia_unit = exp(coef[seq_len(n_items)]),
    direct_dia_sources = "Network model"
  )

  item_value_lookup <- prices %>%
    distinct(item_key) %>%
    with_network_keys() %>%
    left_join(item_values, by = "network_item_key") %>%
    select(item_key, network_item_key, direct_dia_unit, direct_dia_sources)
  curr_values <- tibble(
    curr = c("DIA", curr_levels),
    dia_per_currency = c(1, exp(coef[n_items + seq_len(n_currs)]))
  )

  network_fit <- network_rows %>%
    mutate(
      network_predicted_unit_native = exp(fitted),
      network_log_residual = resid,
      network_weight = weights,
      network_value_ratio = exp(-network_log_residual)
    ) %>%
    transmute(row_id, item_key, network_item_key, network_predicted_unit_native, network_log_residual, network_weight, network_value_ratio)

  anchor_rows <- prices %>%
    with_network_keys() %>%
    filter(curr != "DIA", !is.na(unit_native), unit_native > 0) %>%
    left_join(item_value_lookup, by = c("item_key", "network_item_key")) %>%
    left_join(curr_values, by = "curr") %>%
    left_join(network_fit, by = c("row_id", "item_key", "network_item_key")) %>%
    mutate(
      observed_dia_per_currency = direct_dia_unit / unit_native,
      observed_value_index = 100 * observed_dia_per_currency,
      listing_key = clean_item_key(item),
      network_value_ratio = observed_dia_per_currency / dia_per_currency
    ) %>%
    filter(!is.na(direct_dia_unit), !is.na(dia_per_currency), is.finite(observed_dia_per_currency), observed_dia_per_currency > 0) %>%
    group_by(item_key) %>%
    mutate(item_price_rank = min_rank(desc(network_value_ratio))) %>%
    ungroup() %>%
    group_by(listing_key) %>%
    mutate(
      max_other_listing_limit = vapply(
        seq_along(limit),
        function(i) safe_max(limit[-i]),
        numeric(1)
      )
    ) %>%
    ungroup() %>%
    mutate(
      excluded_on_special = !is.na(limit) & !is.na(max_other_listing_limit) &
        limit < 0.10 * max_other_listing_limit & item_price_rank <= 2 &
        network_value_ratio >= 1.5
    )

  anchors <- anchor_rows %>%
    group_by(curr, store, anchor_group) %>%
    arrange(desc(network_weight), abs(network_log_residual), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup()

  rates <- curr_values %>%
    left_join(
      anchor_rows %>%
        group_by(curr) %>%
        summarise(
          anchor_items = n_distinct(item_key),
          observations = n(),
          min_anchor = min(observed_dia_per_currency, na.rm = TRUE),
          median_anchor = median(observed_dia_per_currency, na.rm = TRUE),
          max_anchor = max(observed_dia_per_currency, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "curr"
    ) %>%
    mutate(
      anchor_items = if_else(curr == "DIA", NA_integer_, anchor_items),
      observations = if_else(curr == "DIA", NA_integer_, observations),
      min_anchor = if_else(curr == "DIA", 1, min_anchor),
      median_anchor = if_else(curr == "DIA", 1, median_anchor),
      max_anchor = if_else(curr == "DIA", 1, max_anchor)
    )

  anchor_candidates <- anchor_rows %>%
    left_join(
      anchors %>% transmute(row_id, selected_anchor = TRUE),
      by = "row_id"
    ) %>%
    mutate(
      selected_anchor = if_else(is.na(selected_anchor), FALSE, selected_anchor),
      excluded_throwaway = network_value_ratio < 0.5,
      excluded_reason = case_when(
        excluded_on_special ~ "On special: network value is high and limit is below 10% of another store's limit",
        !selected_anchor ~ "Duplicate comparable listing: weaker row in same store/item family",
        excluded_throwaway ~ "Costly outlier: less than 0.5x network expectation",
        TRUE ~ ""
      ),
      is_excluded = excluded_reason != ""
    )

  no_direct_candidates <- prices %>%
    filter(curr != "DIA", !is.na(unit_native), unit_native > 0) %>%
    with_network_keys() %>%
    anti_join(item_value_lookup %>% select(item_key), by = "item_key") %>%
    mutate(
      direct_dia_unit = NA_real_,
      direct_dia_sources = "",
      dia_per_currency = NA_real_,
      observed_dia_per_currency = NA_real_,
      observed_value_index = NA_real_,
      network_value_ratio = NA_real_,
      network_log_residual = NA_real_,
      network_weight = NA_real_,
      item_price_rank = NA_integer_,
      excluded_on_special = FALSE,
      selected_anchor = FALSE,
      excluded_throwaway = FALSE,
      excluded_reason = "No network comparison",
      is_excluded = TRUE
    )

  anchor_candidates <- bind_rows(anchor_candidates, no_direct_candidates)

  valued <- prices %>%
    with_network_keys() %>%
    left_join(rates %>% select(curr, dia_per_currency), by = "curr") %>%
    left_join(item_value_lookup, by = c("item_key", "network_item_key")) %>%
    left_join(network_fit, by = c("row_id", "item_key", "network_item_key")) %>%
    mutate(
      effective_dia_unit = unit_native * dia_per_currency,
      direct_value_vs_rate = if_else(
        !is.na(direct_dia_unit) & !is.na(effective_dia_unit) & effective_dia_unit > 0,
        100 * direct_dia_unit / effective_dia_unit,
        NA_real_
      ),
      network_value_ratio = direct_value_vs_rate / 100
    )

  best_known <- valued %>%
    filter(!is.na(effective_dia_unit), effective_dia_unit > 0) %>%
    group_by(item_key) %>%
    summarise(
      best_effective_dia_unit = min(effective_dia_unit),
      best_store = paste(sort(unique(store[effective_dia_unit == min(effective_dia_unit)])), collapse = ", "),
      .groups = "drop"
    )

  valued <- valued %>%
    left_join(best_known, by = "item_key") %>%
    mutate(
      normal_effective_dia_unit = direct_dia_unit,
      priority_score = if_else(
        !is.na(best_effective_dia_unit) & !is.na(effective_dia_unit) & effective_dia_unit > 0,
        100 * best_effective_dia_unit / effective_dia_unit,
        NA_real_
      ),
      normal_value_ratio = if_else(
        !is.na(normal_effective_dia_unit) & !is.na(effective_dia_unit) & effective_dia_unit > 0,
        normal_effective_dia_unit / effective_dia_unit,
        NA_real_
      ),
      total_effective_dia = price * dia_per_currency
    )

  diagnostics <- valued %>%
    filter(!is.na(network_value_ratio)) %>%
    mutate(
      direction = case_when(
        network_value_ratio >= 1.5 ~ "Cheaper than network",
        network_value_ratio <= 0.5 ~ "Costlier than network",
        TRUE ~ "Near network"
      )
    )

  list(rates = rates, anchors = anchors, anchor_candidates = anchor_candidates,
       valued = valued, direct = item_value_lookup, diagnostics = diagnostics)
}

fmt_num <- function(x, digits = 2) {
  out <- ifelse(
    is.na(x),
    "",
    format(round(x, digits), big.mark = ",", nsmall = 0, scientific = FALSE, trim = TRUE)
  )
  out <- ifelse(str_detect(out, "\\."), str_replace(out, "0+$", ""), out)
  str_replace(out, "\\.$", "")
}

fmt_sig <- function(x, digits = 2) {
  out <- ifelse(
    is.na(x),
    "",
    format(signif(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
  out <- ifelse(str_detect(out, "\\."), str_replace(out, "0+$", ""), out)
  str_replace(out, "\\.$", "")
}

round_train_dia <- function(x) {
  nearest <- ifelse(abs(x) < 500, 50, 100)
  ifelse(is.na(x), NA_real_, floor(x / nearest + 0.5) * nearest)
}

fmt_dia_equiv <- function(x) {
  fmt_num(round_train_dia(x), 0)
}

fmt_dia_compact <- function(x) {
  x <- round_train_dia(x)
  ifelse(abs(x) >= 1000, paste0(fmt_num(x / 1000, 1), "k"), fmt_num(x, 0))
}

fmt_limit <- function(limit, store = "") {
  ifelse(
    store == "Diamond Storefront" & is.na(limit),
    "None",
    fmt_num(limit, 0)
  )
}

bold_cell <- function(x) {
  ifelse(x == "" | is.na(x), "", paste0("<strong>", x, "</strong>"))
}

excluded_cell <- function(x, excluded) {
  x <- ifelse(is.na(x), "", x)
  x
}

excluded_short_label <- function(reason) {
  case_when(
    reason == "" ~ "Fits network",
    str_detect(reason, "^On special") ~ "Special",
    str_detect(reason, "^Throwaway|^Costly outlier") ~ "Costly outlier",
    str_detect(reason, "^Duplicate") ~ "Duplicate",
    str_detect(reason, "^No direct|^No network") ~ "No network",
    TRUE ~ reason
  )
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else max(x)
}

dia_unit_col <- function(curr = NA_character_) {
  ifelse(!is.na(curr) & curr == "DIA", "DIA/unit", "~DIA/unit")
}

fmt_game_qty <- function(x) {
  out <- case_when(
    is.na(x) ~ "",
    abs(x) >= 1e9 ~ paste0(fmt_num(x / 1e9, 2), "b"),
    abs(x) >= 1e6 ~ paste0(fmt_num(x / 1e6, 2), "m"),
    abs(x) >= 1e3 ~ paste0(fmt_num(x / 1e3, 2), "k"),
    TRUE ~ fmt_num(x, 2)
  )
  out
}

base_unit_qty <- function(qty, unit) {
  unit_label <- case_when(
    unit == "1h" ~ "1h Speed-Up",
    unit == "8-Hour Shield" ~ "8-Hour Shield",
    unit == "resource" ~ "resource",
    unit == "10k Battle Data" ~ "10k Battle Data",
    unit == "Lv 1 Components" ~ "Lv 1 Component",
    unit == "Lv 1 Component Choices" ~ "Lv 1 Component Choice",
    unit == "Universal Decor Component" ~ "Universal Decor Component",
    unit == "superalloy" ~ "Superalloy",
    unit == "shard" ~ "shard",
    unit == "SR Hero EXP Chest" ~ "SR Hero EXP Chest",
    str_detect(unit, "^(EV:|100% UR \\+)") ~ "Skill Chip Chest",
    TRUE ~ unit
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & str_detect(unit_label, "Chest$"),
    paste0(unit_label, "s"),
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "Universal Decor Component",
    "Universal Decor Components",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "8-Hour Shield",
    "8-Hour Shields",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "1h Speed-Up",
    "1h Speed-Ups",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "Lv 1 Component",
    "Lv 1 Components",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "Lv 1 Component Choice",
    "Lv 1 Component Choices",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "unit",
    "units",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "shard",
    "shards",
    unit_label
  )
  paste(fmt_game_qty(qty), unit_label)
}

deal_badge <- function(score) {
  label <- ifelse(is.na(score), "", paste0(fmt_num(score, 2), "x"))
  cls <- case_when(
    is.na(score) ~ "",
    score >= 1.25 ~ "deal-great",
    score >= 0.85 ~ "deal-mid",
    TRUE ~ "deal-bad"
  )
  ifelse(label == "", "", paste0("<span class='deal-badge ", cls, "'>", label, "</span>"))
}

best_store_cell <- function(best_store, current_store = NA_character_) {
  is_current <- !is.na(best_store) & !is.na(current_store) &
    vapply(strsplit(best_store, ",\\s*"), function(stores) current_store %in% stores, logical(1))
  ifelse(
    is_current,
    paste0("<span class='best-current'>", best_store, "</span>"),
    best_store
  )
}

js_string <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\\\", "\\\\\\\\")
  x <- str_replace_all(x, "'", "\\\\'")
  x <- str_replace_all(x, "[\r\n]+", " ")
  paste0("'", x, "'")
}

table_link_html <- function(html, nav_kind, value, title = "") {
  if (is.na(value) || value == "") return(html)
  href <- paste0("?view=", nav_kind, "&", nav_kind, "=", utils::URLencode(value, reserved = TRUE))
  paste0(
    "<a href='", htmltools::htmlEscape(href), "' class='lw-table-link' title='", htmltools::htmlEscape(title),
    "' onclick='window.location.href=this.href; return false;",
    "' data-nav-kind='", htmltools::htmlEscape(nav_kind),
    "' data-nav-value='", htmltools::htmlEscape(value), "'>", html, "</a>"
  )
}

icon_badge <- function(label, cls = "misc", title = "") {
  paste0(
    "<span class='lw-icon icon-", cls, "' title='", htmltools::htmlEscape(title), "'>",
    htmltools::htmlEscape(label),
    "</span>"
  )
}

icon_img <- function(file, title = "") {
  if (!file.exists(file.path("www", "icons", file))) {
    return("")
  }
  image_size <- ifelse(str_detect(file, "\\.svg$"), "92%", "150%")
  paste0(
    "<span class='lw-icon lw-img' title='", htmltools::htmlEscape(title),
    "' style=\"background-image:url('icons/", htmltools::htmlEscape(file),
    "?v=", icon_cache_bust, "'); background-size:", image_size, ";\"></span>"
  )
}

icon_or_badge <- function(file, label, cls = "misc", title = "") {
  img <- icon_img(file, title)
  ifelse(img == "", icon_badge(label, cls, title), img)
}

item_icon <- function(item, item_key = "") {
  if (length(item) == 0) {
    return(character(0))
  }
  if (length(item) > 1 || length(item_key) > 1) {
    n <- max(length(item), length(item_key))
    return(mapply(
      item_icon,
      rep_len(item, n),
      rep_len(item_key, n),
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    ))
  }

  item_l <- clean_item_key(item)
  key_l <- clean_item_key(item_key)
  component_level <- str_match(item_l, "\\blv\\.?\\s*([0-9]+)\\s*(?:drone\\s*)?component")[, 2]
  case_when(
    item_l == "diamonds" ~ icon_or_badge("diamonds.svg", "DIA", "diamond", "Diamonds"),
    item_l == "alliance contribution purple" ~ icon_or_badge("currency-alliance-contribution-purple.svg", "ALL", "badge", "Alliance Contributions"),
    str_detect(item_l, "alliance contribution") ~ icon_or_badge("currency-alliance-contribution.svg", "ALL", "alliance", "Alliance Contributions"),
    str_detect(item_l, "(drone )?component choice chest") & component_level %in% c("3", "5") ~
      icon_or_badge(paste0("drone-component-choice-chest-lv", component_level, ".svg"), paste0("Lv", component_level), "drone", paste0("Lv.", component_level, " component choice chest")),
    str_detect(item_l, "(drone )?component choice chest") ~ icon_badge(if_else(is.na(component_level), "Lv", paste0("Lv", component_level)), "drone", "Component choice chest"),
    str_detect(item_l, "(drone )?component chest") & component_level %in% c("1", "3", "5") ~
      icon_or_badge(paste0("drone-component-chest-lv", component_level, ".svg"), paste0("Lv", component_level), "drone", paste0("Lv.", component_level, " component chest")),
    str_detect(item_l, "(drone )?component chest") ~ icon_badge(if_else(is.na(component_level), "Lv", paste0("Lv", component_level)), "drone", "Component chest"),
    item_l == "stamina" ~ icon_or_badge("stamina.svg", "STA", "energy", "Stamina"),
    item_l == "advanced teleporter" ~ icon_or_badge("advanced-teleporter.svg", "ADV", "teleport", "Advanced teleporter"),
    item_l == "alliance teleporter" ~ icon_or_badge("alliance-teleporter.svg", "ALL", "teleport", "Alliance teleporter"),
    item_l == "random teleporter" ~ icon_or_badge("random-teleporter.svg", "?", "teleport", "Random teleporter"),
    str_detect(item_l, "8 hour shield|8 h shield") ~ icon_or_badge("shield-8h.svg", "8h", "shield", "8-hour shield"),
    str_detect(item_l, "12 hour shield|12 h shield") ~ icon_or_badge("shield-12h.svg", "12h", "shield", "12-hour shield"),
    str_detect(item_l, "24 hour shield|24 h shield") ~ icon_or_badge("shield-24h.svg", "24h", "shield", "24-hour shield"),
    item_l == "trade contract" ~ icon_badge("TRK", "contract", "Trade contract"),
    item_l == "universal decor component" ~ icon_or_badge("universal-decor-component.svg", "DEC", "decor", "Universal decor component"),
    str_detect(item_l, "decoration chest") ~ icon_or_badge("decoration-chest-ur.svg", "DEC", "decor", "Decoration chest"),
    item_l == "valor badge" ~ icon_or_badge("valor-badge.webp", "VAL", "badge", "Valor badge"),
    item_l == "survivor s token" ~ icon_or_badge("survivor-token.svg", "TOK", "token", "Survivor's Token"),
    item_l == "diamonds" ~ icon_or_badge("diamonds.svg", "DIA", "diamond", "Diamonds"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("skill-chip-chest-ur.svg", "UR", "chip", "Skill Chip Chest (UR)"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("skill-chip-chest-ssr.svg", "SSR", "chip", "Skill Chip Chest (SSR)"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\bsr\\b") ~ icon_or_badge("skill-chip-chest-sr.svg", "SR", "chip", "Skill Chip Chest (SR)"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\br\\b") ~ icon_or_badge("skill-chip-chest-r.svg", "R", "chip", "Skill Chip Chest (R)"),
    str_detect(item_l, "skill chip chest") ~ icon_or_badge("skill-chip-chest-r.svg", "CHP", "chip", "Skill chip chest"),
    item_l == "basic chip material" ~ icon_or_badge("basic-chip-material.webp", "BAS", "chip", "Basic chip material"),
    item_l == "premium chip material" ~ icon_or_badge("premium-chip-material.webp", "PRM", "chip", "Premium chip material"),
    item_l == "skill medal" ~ icon_or_badge("skill-medal.webp", "MED", "medal", "Skill medal"),
    str_detect(item_l, "hero exp") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("hero-exp-ur.svg", "EXP", "hero", "Hero EXP Chest (UR)"),
    str_detect(item_l, "hero exp") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("hero-exp-ssr.svg", "EXP", "hero", "Hero EXP Chest (SSR)"),
    str_detect(item_l, "hero exp") & str_detect(item_l, "\\bsr\\b") ~ icon_or_badge("hero-exp-sr.svg", "EXP", "hero", "Hero EXP Chest (SR)"),
    str_detect(item_l, "hero exp") ~ icon_or_badge("hero-exp-ssr.svg", "EXP", "hero", "Hero EXP"),
    item_l == "profession exp" ~ icon_or_badge("profession-exp.svg", "EXP", "hero", "Profession EXP"),
    item_l == "recruitment orders" ~ icon_or_badge("recruitment-orders.svg", "ORD", "ticket", "Recruitment Orders"),
    item_l == "s1 skill point" ~ icon_or_badge("season-s1-skill-point.svg", "S1", "badge", "S1 Skill Point"),
    item_l == "mutant crystals" ~ icon_or_badge("mutant-crystals.svg", "MUT", "material", "Mutant Crystals"),
    item_l == "dominance sanctuary permanent" ~ icon_or_badge("season-dominance-sanctuary.svg", "SAN", "decor", "Dominance Sanctuary (Permanent)"),
    item_l == "god of judgment" ~ icon_or_badge("season-god-of-judgment.svg", "GOD", "decor", "God of Judgment"),
    item_l == "profession change certificate" ~ icon_or_badge("season-profession-change.svg", "JOB", "badge", "Profession Change Certificate"),
    item_l == "profession skill reset book" ~ icon_or_badge("season-profession-reset.svg", "SKL", "badge", "Profession Skill Reset Book"),
    item_l == "sandstorm master permanent" ~ icon_or_badge("season-sandstorm-master.svg", "SAN", "decor", "Sandstorm Master (Permanent)"),
    item_l == "ur hero badge" ~ icon_or_badge("season-ur-hero-badge.svg", "UR", "badge", "UR Hero Badge"),
    item_l == "s1 gift chest" ~ icon_or_badge("s1-gift-chest.svg", "S1", "chest", "S1 Gift Chest"),
    item_l == "ssr gear chest" ~ icon_or_badge("gear-chest-ssr.svg", "SSR", "gear", "SSR Gear Chest"),
    item_l == "gear chest sr" ~ icon_or_badge("gear-chest-sr.svg", "SR", "gear", "Gear Chest (SR)"),
    item_l == "gear chest r" ~ icon_or_badge("gear-chest-r.svg", "R", "gear", "Gear Chest (R)"),
    item_l == "ur campaign chest" | item_l == "campaign chest ur" ~ icon_or_badge("campaign-chest-ur.svg", "UR", "chest", "UR Campaign Chest"),
    item_l == "luxury choice chest" ~ icon_or_badge("choice-chest-luxury.svg", "LUX", "chest", "Luxury Choice Chest"),
    item_l == "deluxe choice chest" ~ icon_or_badge("choice-chest-deluxe.svg", "DEL", "chest", "Deluxe Choice Chest"),
    item_l == "hero choice chest" ~ icon_or_badge("hero-choice-chest.png", "HERO", "chest", "Hero Choice Chest"),
    item_l == "dual propeller base" ~ icon_or_badge("dual-propeller-base.png", "DPB", "decor", "Dual Propeller Base"),
    item_l == "tower of victory ur deco" ~ icon_or_badge("tower-of-victory.png", "TWR", "decor", "Tower of Victory"),
    item_l == "sr food iron coin chest" ~ icon_or_badge("resource-chest-sr-combo.svg", "SR", "chest", "SR Food/Iron/Coin Chest"),
    item_l == "resource chest sr" ~ icon_or_badge("resource-chest-sr.svg", "SR", "chest", "Resource Chest (SR)"),
    item_l == "hero recruitment ticket" ~ icon_or_badge("hero-recruitment-ticket.svg", "HRT", "ticket", "Hero recruitment ticket"),
    item_l == "survivor recruitment ticket" ~ icon_or_badge("survivor-ticket.webp", "SRT", "ticket", "Survivor recruitment ticket"),
    item_l == "universal exclusive weapon shard" ~ icon_or_badge("universal-exclusive-weapon-shard.svg", "WPN", "shard", "Universal Exclusive Weapon Shard"),
    str_detect(item_l, "\\bur\\b.*hero.*shard|hero choice chest|universal ur hero shard") ~ icon_or_badge("shard-ur.svg", "UR", "shard", "UR hero shard"),
    str_detect(item_l, "\\bssr\\b.*hero.*shard|violet shard") ~ icon_or_badge("shard-ssr.svg", "SSR", "shard", "SSR hero shard"),
    str_detect(item_l, "\\bsr\\b.*hero.*shard") ~ icon_or_badge("shard-sr.svg", "SR", "shard", "SR hero shard"),
    str_detect(item_l, "hero universal shard|hero shard") ~ icon_or_badge("shard-ur.svg", "SHD", "shard", "Hero shard"),
    item_l == "5 minute speed up chest" | item_l == "5 min speed up chest" | item_l == "5m speed up chest" | item_l == "5m speed-up chest" ~ icon_or_badge("speed-chest-5m.svg", "5m", "speed", "5-minute speed-up chest"),
    str_detect(item_l, "5m research speed up") ~ icon_or_badge("speed-research-5m.svg", "5m", "speed", "5m research speed-up"),
    str_detect(item_l, "5m construction speed up") ~ icon_or_badge("speed-construction-5m.svg", "5m", "speed", "5m construction speed-up"),
    str_detect(item_l, "research speed up") ~ icon_or_badge("speed-research.svg", "R&D", "speed", "Research speed-up"),
    str_detect(item_l, "training speed up") ~ icon_or_badge("speed-training.svg", "TRN", "speed", "Training speed-up"),
    str_detect(item_l, "construction speed up") ~ icon_or_badge("speed-construction.svg", "BLD", "speed", "Construction speed-up"),
    str_detect(item_l, "healing speed up") ~ icon_or_badge("speed-healing.svg", "HL", "speed", "Healing speed-up"),
    str_detect(item_l, "universal speed up|\\bspeed up\\b") ~ icon_or_badge("speed-universal.svg", ">>", "speed", "Universal speed-up"),
    str_detect(item_l, "resource choice chest") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("resource-choice-chest-ur.svg", "UR", "chest", "Resource Choice Chest (UR)"),
    str_detect(item_l, "resource choice chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("resource-choice-chest-ssr.svg", "SSR", "chest", "Resource Choice Chest (SSR)"),
    str_detect(item_l, "resource choice chest") ~ icon_or_badge("resource-choice-chest-ssr.svg", "RSC", "chest", "Resource choice chest"),
    str_detect(item_l, "food chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("food-chest-ssr.svg", "FD", "food", "SSR Food Chest"),
    str_detect(item_l, "iron chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("iron-chest-ssr.svg", "FE", "iron", "SSR Iron Chest"),
    str_detect(item_l, "coin chest") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("coin-chest-ur.svg", "$", "coin", "UR Coin Chest"),
    str_detect(item_l, "coin chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("coin-chest-ssr.svg", "$", "coin", "SSR Coin Chest"),
    item_l == "food" | str_detect(key_l, "food resource") ~ icon_or_badge("food-50k.svg", "FD", "food", "Food resource"),
    item_l == "iron" | str_detect(key_l, "iron resource") ~ icon_or_badge("iron-50k.svg", "FE", "iron", "Iron resource"),
    item_l == "coins" | str_detect(key_l, "coins resource") ~ icon_or_badge("coins-18k.svg", "$", "coin", "Coins resource"),
    str_detect(item_l, "food chest") ~ icon_badge("FD", "food", "Food"),
    str_detect(item_l, "iron chest") ~ icon_badge("FE", "iron", "Iron"),
    str_detect(item_l, "coin chest") ~ icon_badge("$", "coin", "Coins"),
    item_l == "drone parts" ~ icon_or_badge("drone-parts.webp", "GER", "drone", "Drone parts"),
    str_detect(item_l, "drone combat boost") ~ icon_or_badge("drone-combat-boost.svg", "EXP", "drone", "Drone Combat Boost EXP"),
    str_detect(item_l, "^battle data$|battle data 10k|battle data 100k") ~ icon_or_badge("battle-data.webp", "DAT", "drone", "Drone data"),
    str_detect(item_l, "gear blueprint") & str_detect(item_l, "\\bmr\\b") ~ icon_or_badge("gear-blueprint-mr.svg", "MR", "gear", "MR gear blueprint"),
    str_detect(item_l, "gear blueprint") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("gear-blueprint-ur.svg", "UR", "gear", "UR gear blueprint"),
    str_detect(item_l, "gear blueprint") ~ icon_or_badge("gear-blueprint-ur.svg", "BP", "gear", "Gear blueprint"),
    str_detect(item_l, "dielectric ceramic") ~ icon_or_badge("dielectric-ceramic.webp", "CER", "material", "Dielectric ceramic"),
    item_l == "synthetic resin" ~ icon_or_badge("synthetic-resin.webp", "RES", "material", "Synthetic resin"),
    item_l == "superalloy" ~ icon_or_badge("superalloy.svg", "ALY", "material", "Superalloy"),
    item_l == "upgrade ore" ~ icon_or_badge("upgrade-ore.svg", "ORE", "ore", "Upgrade ore"),
    TRUE ~ icon_badge("·", "misc", "Item")
  )
}

currency_icon <- function(curr) {
  case_when(
    curr == "DIA" ~ icon_or_badge("diamonds.svg", "DIA", "diamond", "Diamonds"),
    curr == "ALL" ~ icon_or_badge("currency-alliance-contribution.svg", "ALL", "alliance", "Alliance Contributions"),
    curr == "CAM" ~ icon_or_badge("currency-campaign-medal.svg", "CAM", "campaign", "Campaign Medals"),
    curr == "COUR" ~ icon_or_badge("currency-courage-medal.svg", "CRG", "courage", "Courage Medals"),
    curr == "GLIT" ~ icon_or_badge("currency-glitter-coin.svg", "GL", "glitter", "Glitter Coins"),
    curr == "BOUN" ~ icon_or_badge("currency-bounty-voucher.svg", "BV", "bounty", "Bounty Vouchers"),
    curr == "HON" ~ icon_or_badge("currency-honor.svg", "HON", "honor", "Honor Points"),
    curr == "MOB" ~ icon_badge("MOB", "campaign", "Total Mobilization"),
    curr == "ID" ~ icon_badge("ID", "badge", "ID Points"),
    curr == "SER" ~ icon_badge("SER", "material", "Serum Fragments"),
    curr == "SEA" ~ icon_badge("SEA", "campaign", "Season Currency"),
    curr == "DEL" ~ icon_or_badge("choice-chest-deluxe.svg", "DEL", "chest", "Deluxe Choice Chest pick"),
    curr == "LUX" ~ icon_or_badge("choice-chest-luxury.svg", "LUX", "chest", "Luxury Choice Chest pick"),
    TRUE ~ icon_badge(curr, "misc", curr)
  )
}

item_cell <- function(item, item_key = "") {
  paste0("<span class='icon-cell'>", item_icon(item, item_key), "<span>", htmltools::htmlEscape(item), "</span></span>")
}

train_item_cell <- function(label, icon_item = label) {
  paste0("<span class='icon-cell'>", item_icon(icon_item), "<span>", htmltools::htmlEscape(label), "</span></span>")
}

train_summary_cell <- function(value, tier) {
  cls <- paste("train-summary-cell", paste0("train-summary-", tier))
  paste0("<span class='", cls, "'>", value, "</span>")
}

item_link_cell <- function(item, item_key = "", target_item = "") {
  mapply(
    function(item_one, key_one, target_one) {
      table_link_html(item_cell(item_one, key_one), "item", target_one, "Open item view")
    },
    item, item_key, target_item,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
}

store_link_cell <- function(store) {
  vapply(store, function(store_one) {
    table_link_html(htmltools::htmlEscape(store_one), "store", store_one, "Open storefront")
  }, character(1))
}

store_links_cell <- function(stores, current_store = NA_character_) {
  current_store <- ifelse(length(current_store) == 0, NA_character_, current_store[1])
  vapply(stores, function(stores_one) {
    if (is.na(stores_one) || stores_one == "") return("")
    parts <- str_split(stores_one, ",\\s*", simplify = FALSE)[[1]]
    links <- vapply(parts, function(store) {
      html <- htmltools::htmlEscape(store)
      if (!is.na(current_store) && store == current_store) {
        html <- paste0("<span class='best-current'>", html, "</span>")
      }
      table_link_html(html, "store", store, "Open storefront")
    }, character(1))
    paste(links, collapse = ", ")
  }, character(1))
}

best_dia_store_cell <- function(value, stores) {
  paste0(fmt_num(value, 2), " (", store_links_cell(stores), ")")
}

currency_anchor_link <- function(curr) {
  vapply(curr, function(curr_one) {
    if (is.na(curr_one) || curr_one == "DIA") return("")
    href <- paste0("?view=model&currency=", utils::URLencode(curr_one, reserved = TRUE))
    paste0(
      "<a href='", htmltools::htmlEscape(href),
      "' class='lw-table-link' title='View network observations' onclick='window.location.href=this.href; return false;'>View</a>"
    )
  }, character(1))
}

currency_cell <- function(curr) {
  paste0("<span class='icon-cell'>", currency_icon(curr), "<span>", curr, "</span></span>")
}

currency_label <- function(curr) {
  labels <- c(
    DIA = "Diamonds",
    ALL = "Alliance Contributions",
    HON = "Honor Points",
    CAM = "Campaign Medals",
    MOB = "Total Mobilization",
    ID = "ID Points",
    SER = "Serum Fragments",
    SEA = "Season Currency",
    DEL = "Deluxe Choice Chest pick",
    LUX = "Luxury Choice Chest pick",
    COUR = "Courage Medals",
    BOUN = "Bounty Hunter Tokens",
    GLIT = "Glittering Market Tokens"
  )
  out <- labels[curr]
  ifelse(is.na(out), curr, out)
}

train_items <- function(hq_level = 29) {
  sr_food <- resource_chest_amounts(hq_level) %>%
    filter(resource == "food", tier == "sr") %>%
    pull(amount)
  if (!length(sr_food) || is.na(sr_food)) {
    sr_food <- resource_chest_amounts(29) %>%
      filter(resource == "food", tier == "sr") %>%
      pull(amount)
  }

  top_tier_ids <- c(
    "battle_data_10",
    "gear_ssr_1",
    "ur_decoration_1",
    "ur_resource_choice_3",
    "ur_coin_chest_5",
    "skill_medal_3000",
    "upgrade_ore_2500",
    "dielectric_ceramic_50",
    "drone_parts_6",
    "ur_hero_shard_2"
  )
  low_tier_ids <- c(
    "alliance_contribution_500",
    "alliance_contribution_1000",
    "alliance_contribution_1500",
    "battle_data_4",
    "diamonds_50",
    "drone_parts_2",
    "drone_component_lv1_1",
    "speed_5m_10",
    "speed_5m_15",
    "skill_medal_1800",
    "sr_hero_exp_8",
    "sr_hero_exp_16",
    "upgrade_ore_500",
    "sr_resource_chest_4",
    "sr_resource_chest_8",
    "sr_resource_chest_16",
    "ssr_coin_chest_1",
    "ssr_coin_chest_3",
    "ssr_hero_shard_1",
    "resource_chest_25",
    "resource_chest_40",
    "ssr_coin_chest_2",
    "sr_resource_chest_24",
    "gear_r_1",
    "upgrade_ore_1000"
  )

  tibble::tribble(
    ~id, ~label, ~icon_item, ~reward_qty, ~value_kind, ~item_key, ~comparable_qty, ~currency, ~flat_dia,
    "battle_data_10", "10k Battle Data (x10)", "Battle Data (10k)", "10", "item", "battle data", 10, NA_character_, NA_real_,
    "battle_data_5", "10k Battle Data (x5)", "Battle Data (10k)", "5", "item", "battle data", 5, NA_character_, NA_real_,
    "hero_ticket_1", "Hero Recruitment Ticket (x1)", "Hero Recruitment Ticket", "1", "item", "hero recruitment ticket", 1, NA_character_, NA_real_,
    "resource_chest_50", "Resource Chest (x50)", "Resource Chest (SR)", "50", "item", "food resource", 50 * 10000 / sr_food, NA_character_, NA_real_,
    "speed_5m_20", "5-min Speed Up Chest (x20)", "5m Speed Up Chest", "20", "item", "construction speed up hour", 20 * 5 / 60, NA_character_, NA_real_,
    "diamonds_100", "100 Diamonds", "Diamonds", "100", "flat", NA_character_, 1, NA_character_, 100,
    "gear_ssr_1", "SSR Gear Chest (x1)", "SSR Gear Chest", "1", "item", "superalloy equivalent", 150 * 4, NA_character_, NA_real_,
    "gear_sr_1", "Gear Chest (SR) (x1)", "Gear Chest SR", "1", "item", "superalloy equivalent", 40, NA_character_, NA_real_,
    "ur_decoration_1", "UR Decoration Chest (x1)", "Decoration Chest (UR)", "1", "item", "universal decor component equivalent", 130, NA_character_, NA_real_,
    "alliance_contribution_2500", "Alliance Contribution (x2.5k)", "Alliance Contribution", "2.5k", "currency", NA_character_, 2500, "ALL", NA_real_,
    "ur_resource_choice_3", "UR Resource Choice Chest (x3)", "Resource Choice Chest (UR)", "3", "item", "food resource", 3 * resource_tier_multiplier("ur"), NA_character_, NA_real_,
    "ur_coin_chest_5", "UR Coin Chest (x5)", "UR Coin Chest", "5", "item", "coins resource", 5 * resource_tier_multiplier("ur"), NA_character_, NA_real_,
    "ur_hero_shard_1", "UR Universal Hero Shard (x1)", "UR Hero Universal Shard", "1", "item", "ur hero shard equivalent", 1, NA_character_, NA_real_,
    "sr_hero_exp_32", "SR Hero EXP Chest (x32)", "Hero EXP Chest (SR)", "32", "item", "hero exp chest sr equivalent", 32, NA_character_, NA_real_,
    "sr_resource_chest_32", "SR Food/Iron/Coin Chest (x32)", "SR Food/Iron/Coin Chest", "32", "item", "coins resource", 32 * resource_tier_multiplier("sr"), NA_character_, NA_real_,
    "ssr_coin_chest_5", "SSR Coin Chest (x5)", "SSR Coin Chest", "5", "item", "coins resource", 5 * resource_tier_multiplier("ssr"), NA_character_, NA_real_
    ,
    "drone_parts_3", "Drone Parts (x3)", "Drone Parts", "3", "item", "drone parts", 3, NA_character_, NA_real_,
    "drone_parts_6", "Drone Parts (x6)", "Drone Parts", "6", "item", "drone parts", 6, NA_character_, NA_real_,
    "upgrade_ore_2000", "Upgrade Ore (x2.0k)", "Upgrade Ore", "2.0k", "item", "upgrade ore", 2000, NA_character_, NA_real_,
    "upgrade_ore_2500", "Upgrade Ore (x2.5k)", "Upgrade Ore", "2.5k", "item", "upgrade ore", 2500, NA_character_, NA_real_,
    "drone_component_lv3_1", "Lv.3 Drone Component Chest (x1)", "Lv.3 Drone Component Chest", "1", "item", "drone component level 1 equivalent", 9, NA_character_, NA_real_,
    "ur_hero_shard_2", "UR Universal Hero Shard (x2)", "UR Hero Universal Shard", "2", "item", "ur hero shard equivalent", 2, NA_character_, NA_real_,
    "skill_medal_2400", "Skill Medal (x2.4k)", "Skill Medal", "2.4k", "item", "skill medal", 2400, NA_character_, NA_real_,
    "skill_medal_3000", "Skill Medal (x3.0k)", "Skill Medal", "3.0k", "item", "skill medal", 3000, NA_character_, NA_real_,
    "universal_decor_component_20", "Universal Decor Component (x20)", "Universal Decor Component", "20", "item", "universal decor component equivalent", 20, NA_character_, NA_real_,
    "dielectric_ceramic_50", "Dielectric Ceramic (x50)", "Dielectric Ceramic", "50", "item", "superalloy equivalent", 50 * 16, NA_character_, NA_real_,
    "alliance_contribution_500", "Alliance Contribution (x500)", "Alliance Contribution Purple", "500", "currency", NA_character_, 500, "ALL", NA_real_,
    "alliance_contribution_1000", "Alliance Contribution (x1.0k)", "Alliance Contribution Purple", "1.0k", "currency", NA_character_, 1000, "ALL", NA_real_,
    "alliance_contribution_1500", "Alliance Contribution (x1.5k)", "Alliance Contribution Purple", "1.5k", "currency", NA_character_, 1500, "ALL", NA_real_,
    "battle_data_4", "10k Battle Data (x4)", "Battle Data (10k)", "4", "item", "battle data", 4, NA_character_, NA_real_,
    "diamonds_50", "Diamonds (x50)", "Diamonds", "50", "flat", NA_character_, 1, NA_character_, 50,
    "drone_parts_2", "Drone Parts (x2)", "Drone Parts", "2", "item", "drone parts", 2, NA_character_, NA_real_,
    "drone_component_lv1_1", "Lv.1 Drone Component Chest (x1)", "Lv.1 Drone Component Chest", "1", "item", "drone component level 1 equivalent", 1, NA_character_, NA_real_,
    "speed_5m_10", "5-min Speed Up Chest (x10)", "5m Speed Up Chest", "10", "item", "construction speed up hour", 10 * 5 / 60, NA_character_, NA_real_,
    "speed_5m_15", "5-min Speed Up Chest (x15)", "5m Speed Up Chest", "15", "item", "construction speed up hour", 15 * 5 / 60, NA_character_, NA_real_,
    "skill_medal_1800", "Skill Medal (x1.8k)", "Skill Medal", "1.8k", "item", "skill medal", 1800, NA_character_, NA_real_,
    "sr_hero_exp_8", "SR Hero EXP Chest (x8)", "Hero EXP Chest (SR)", "8", "item", "hero exp chest sr equivalent", 8, NA_character_, NA_real_,
    "sr_hero_exp_16", "SR Hero EXP Chest (x16)", "Hero EXP Chest (SR)", "16", "item", "hero exp chest sr equivalent", 16, NA_character_, NA_real_,
    "upgrade_ore_500", "Upgrade Ore (x500)", "Upgrade Ore", "500", "item", "upgrade ore", 500, NA_character_, NA_real_,
    "sr_resource_chest_4", "SR Food/Iron/Coin Chest (x4)", "SR Food/Iron/Coin Chest", "4", "item", "coins resource", 4 * resource_tier_multiplier("sr"), NA_character_, NA_real_,
    "sr_resource_chest_8", "SR Food/Iron/Coin Chest (x8)", "SR Food/Iron/Coin Chest", "8", "item", "coins resource", 8 * resource_tier_multiplier("sr"), NA_character_, NA_real_,
    "sr_resource_chest_16", "SR Food/Iron/Coin Chest (x16)", "SR Food/Iron/Coin Chest", "16", "item", "coins resource", 16 * resource_tier_multiplier("sr"), NA_character_, NA_real_,
    "ssr_coin_chest_1", "SSR Coin Chest (x1)", "SSR Coin Chest", "1", "item", "coins resource", resource_tier_multiplier("ssr"), NA_character_, NA_real_,
    "ssr_coin_chest_3", "SSR Coin Chest (x3)", "SSR Coin Chest", "3", "item", "coins resource", 3 * resource_tier_multiplier("ssr"), NA_character_, NA_real_,
    "ssr_hero_shard_1", "SSR Universal Hero Shard (x1)", "SSR Hero Universal Shard", "1", "item", "ssr hero shard equivalent", 1, NA_character_, NA_real_,
    "resource_chest_25", "Resource Chest (x25)", "Resource Chest (SR)", "25", "item", "food resource", 25 * 10000 / sr_food, NA_character_, NA_real_,
    "resource_chest_40", "Resource Chest (x40)", "Resource Chest (SR)", "40", "item", "food resource", 40 * 10000 / sr_food, NA_character_, NA_real_,
    "ssr_coin_chest_2", "SSR Coin Chest (x2)", "SSR Coin Chest", "2", "item", "coins resource", 2 * resource_tier_multiplier("ssr"), NA_character_, NA_real_,
    "sr_resource_chest_24", "SR Food/Iron/Coin Chest (x24)", "SR Food/Iron/Coin Chest", "24", "item", "coins resource", 24 * resource_tier_multiplier("sr"), NA_character_, NA_real_,
    "gear_r_1", "Gear Chest (R) (x1)", "Gear Chest (R)", "1", "item", "superalloy equivalent", 10, NA_character_, NA_real_,
    "upgrade_ore_1000", "Upgrade Ore (x1.0k)", "Upgrade Ore", "1.0k", "item", "upgrade ore", 1000, NA_character_, NA_real_
  ) %>%
    mutate(
      tier = case_when(
        id %in% top_tier_ids ~ "top",
        id %in% low_tier_ids ~ "low",
        TRUE ~ "standard"
      ),
      tier_rank = case_when(
        tier == "top" ~ 1L,
        tier == "standard" ~ 2L,
        TRUE ~ 3L
      ),
      tier_sort_qty = if_else(str_detect(str_to_lower(reward_qty), "k"), parse_num(reward_qty) * 1000, parse_num(reward_qty)),
      original_order = row_number(),
      label_family = str_remove(label, "\\s*\\(x[^)]*\\)$")
    ) %>%
    arrange(tier_rank, str_to_lower(label_family), desc(tier_sort_qty), str_to_lower(label))
}

item_choices <- function(prices_df) {
  values <- sort(unique(prices_df$item_canonical))
  labels <- item_menu_label(values)
  stats::setNames(values, labels)
}

initial_prices <- load_prices(hq_level = 29, season = "Season 1")
initial_train_items <- train_items(hq_level = 29)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?v=", icon_cache_bust)),
    tags$script(src = paste0("train.js?v=", icon_cache_bust))
  ),
  div(class = "app-shell",
      div(class = "title-row",
          tags$a(
            class = "app-logo-link",
            href = "https://analyticsff.shinyapps.io/last-war-store-guide/",
            title = "Last War Store Guide home",
            tags$img(
              class = "app-logo",
              src = paste0("icons/last-war-logo.png?v=", icon_cache_bust),
              alt = "Last War"
            )
          ),
          div(class = "title-copy",
              h1("Last War Store Guide"),
              div(class = "subtitle",
                  "Compare store listings after translating each currency into diamond-equivalent value. The conversion model uses overlapping items and an adjustable exchange-rate strictness setting, so weak deals do not dominate the exchange rate."),
              div(class = "byline", "Compliments of [tAf]TheMathNinja [Server 2143]. DM with issues or suggestions."),
              div(class = "build-stamp", app_build_label)
          )
      ),
      div(class = "control-card hq-card",
          selectInput("season", "Season", choices = season_choices, selected = "Season 1"),
          div(class = "note",
              "Season controls which store listings are available throughout the app.")
      ),
      div(class = "control-card hq-card",
          sliderInput("hq_level", "HQ Level", min = 20, max = 30, value = 29, step = 1),
          div(class = "note",
              "This currently only affects the reported value of buying loose resources directly in the Diamond Storefront, which is usually a bad deal. Resource chest multipliers use fixed tier ratios; unentered HQ levels fall back to HQ 29 values.")
      ),
      tabsetPanel(id = "main_tabs",
        tabPanel("Store View",
                 fluidRow(
                   column(3, div(class = "control-card",
                                 selectInput("store", "Store", choices = sort(unique(initial_prices$store))),
                                 div(class = "note",
                                     "Values come from the network conversion model, which fits item and currency values from all store relationships together.")
                   )),
                   column(9,
                          fluidRow(
                            column(4, div(class = "metric", div(class = "label currency-name", textOutput("store_currency_label", inline = TRUE)), div(class = "value", textOutput("store_rate", inline = TRUE)))),
                            column(4, div(class = "metric", div(class = "label", "Converted listings"), div(class = "value", textOutput("store_count", inline = TRUE)))),
                            column(4, div(class = "metric", div(class = "label", "Top item"), div(class = "value", textOutput("store_top", inline = TRUE))))
                          ),
                          tableOutput("store_table")
                   )
                 )),
        tabPanel("Item View",
                 fluidRow(
                   column(3, div(class = "control-card",
                                 selectInput("item", "Item", choices = item_choices(initial_prices)),
                                 div(class = "note",
                                     "Universal/flexible items appear in related item views when they can satisfy the same need.")
                   )),
                   column(9,
                          fluidRow(
                            column(3, div(class = "metric", div(class = "label", "Best Store"), div(class = "value", uiOutput("item_best_store", inline = TRUE)))),
                            column(3, div(class = "metric", div(class = "label", "Best DIA/unit"), div(class = "value", textOutput("item_best_dia", inline = TRUE)))),
                            column(3, div(class = "metric", div(class = "label", "Normal DIA/unit"), div(class = "value", textOutput("item_normal_dia", inline = TRUE)))),
                            column(3, div(class = "metric", div(class = "label", "DIA/unit range"), div(class = "value", textOutput("item_dia_range", inline = TRUE))))
                          ),
                          tableOutput("item_table")
                   )
                 )),
        tabPanel("Train Calculator",
                 div(class = "control-card note",
                     "Enter how many train slots contain each reward. A train car should total exactly 6 item slots.",
                     tags$br(),
                     "This tool cannot suggest the best train car for maximizing in-game power; it only shows equivalent resource cost for a given train car across in-game stores."),
                 fluidRow(
                   column(3, div(class = "metric", div(class = "label", "Items Loaded"), div(class = "value", textOutput("train_slot_count", inline = TRUE)))),
                   column(3, div(class = "metric", div(class = "label", "Train car value"), div(class = "value", textOutput("train_total_dia", inline = TRUE)))),
                   column(3, div(class = "metric", div(class = "label", "Selection probability"), div(class = "value", textOutput("train_selection_probability", inline = TRUE)))),
                   column(3, div(class = "metric", div(class = "label", "Queue position EV"), div(class = "value", textOutput("train_queue_ev", inline = TRUE))))
                 ),
                 uiOutput("train_warning"),
                 uiOutput("train_save_control"),
                 fluidRow(
                   column(6,
                          div(class = "control-card hq-card",
                              numericInput("train_queue", "Commanders in Queue", value = 5, min = 0, max = 100, step = 1),
                              div(class = "note", "Already waiting, before you join.")
                          ),
                          tableOutput("train_table")),
                   column(6, uiOutput("saved_train_cars"))
                 ),
                 uiOutput("train_inputs")),
        tabPanel("Stamina",
                 conditionalPanel(
                   condition = "input.season == 'Preseason'",
                   div(class = "control-card note",
                       strong("Season 1 only."),
                       tags$br(),
                       "Doom Elite stamina purchase values are currently entered only for Season 1.")
                 ),
                 conditionalPanel(
                   condition = "input.season != 'Preseason'",
                   div(class = "control-card note",
                       "This calculator helps you determine the threshold at which Stamina is worth buying directly with diamonds each day, depending on opportunities for spending Stamina (Doom Elite being the most common)."),
                   fluidRow(
                     column(4,
                            div(class = "control-card",
                                checkboxInput("stam_arms_race", "Include Arms Race Bonuses?", value = FALSE),
                                checkboxInput("stam_monica", "Include Monica's Treasure Hunter Lvl 26?", value = FALSE),
                                uiOutput("stam_bonus_note")
                            )),
                     column(8,
                            fluidRow(
                              column(4, div(class = "metric", div(class = "label", "Stamina cost"), div(class = "value", "20"))),
                              column(4, div(class = "metric", div(class = "label", "Total DIA value"), div(class = "value", textOutput("stam_total_value", inline = TRUE)))),
                              column(4, div(class = "metric", div(class = "label", "DIA / Stam"), div(class = "value", textOutput("stam_value_per_stam", inline = TRUE))))
                            ))
                   ),
                   tableOutput("stam_table"),
                   h3("Doom Elite Daily Stamina Purchases"),
                   tableOutput("stam_purchase_table"),
                   uiOutput("arms_race_bonus_heading"),
                   tableOutput("arms_race_bonus_table")
                 )),
        tabPanel("Currency Conversion Model",
                 h3("Inferred Exchange Rates"),
                 tableOutput("rates_table"),
                 div(class = "model-explainer",
                     strong("How the network model works"),
                     div("The model estimates item values and currency exchange rates together from all connected store listings, with DIA fixed at 1."),
                     div("Each listing contributes an observation: item value is expected to equal listed base-unit price times currency value."),
                     div("Observation weight reflects available base-unit volume, softened with a square root and capped so high-limit rows matter more without dominating."),
                     div("VIP, Alliance, Campaign, ID Points Mall, Luxury Choice Chest, and Deluxe Choice Chest are treated as weekly sources, so their listed volume counts as 4x before the square-root softening is applied."),
                     div("Rows far above or below the fitted network remain visible as diagnostics; they are not removed before the first fit.")),
                 div(class = "filter-bar",
                     selectInput("anchor_currency", "Observation currency",
                                 choices = c("All currencies" = "__ALL__", sort(unique(initial_prices$curr[initial_prices$curr != "DIA"]))))),
                 h3("Network Observations"),
                 tableOutput("anchors_table"),
                 h3("Network Diagnostics"),
                 tableOutput("network_table")),
        tabPanel("All Store Listings",
                 div(class = "control-card note",
                     "This is the normalized source table from the workbook. Item keys smooth out a few spelling and naming variants so equivalent goods compare together."),
                 tableOutput("raw_table"))
      )
  )
)

server <- function(input, output, session) {
  saved_train_cars <- reactiveVal(list())
  editing_train_car <- reactiveVal(NULL)

  selected_season <- reactive({
    if (is.null(input$season) || is.na(input$season) || input$season == "") "Season 1" else input$season
  })

  prices <- reactive(load_prices(hq_level = input$hq_level, season = selected_season()))
  model_for <- function() build_value_model(prices())

  store_model <- reactive(model_for())
  item_model <- reactive(model_for())
  currency_model <- reactive(model_for())

  observeEvent(prices(), {
    store_choices <- sort(unique(prices()$store))
    selected_store <- if (!is.null(input$store) && input$store %in% store_choices) input$store else first(store_choices)
    item_choice_values <- item_choices(prices())
    selected_item <- if (!is.null(input$item) && input$item %in% unname(item_choice_values)) input$item else first(unname(item_choice_values))
    updateSelectInput(session, "store", choices = store_choices, selected = selected_store)
    updateSelectInput(session, "item", choices = item_choice_values, selected = selected_item)
    currency_choices <- c("All currencies" = "__ALL__", sort(unique(prices()$curr[prices()$curr != "DIA"])))
    selected_currency <- if (!is.null(input$anchor_currency) && input$anchor_currency %in% unname(currency_choices)) input$anchor_currency else "__ALL__"
    updateSelectInput(session, "anchor_currency", choices = currency_choices, selected = selected_currency)
  }, ignoreInit = TRUE)

  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    if (identical(query$view, "item") && !is.null(query$item) && query$item %in% prices()$item_canonical) {
      updateSelectInput(session, "item", selected = query$item)
      updateTabsetPanel(session, "main_tabs", selected = "Item View")
    }
    if (identical(query$view, "store") && !is.null(query$store) && query$store %in% prices()$store) {
      updateSelectInput(session, "store", selected = query$store)
      updateTabsetPanel(session, "main_tabs", selected = "Store View")
    }
    if (identical(query$view, "model")) {
      if (!is.null(query$currency) && query$currency %in% prices()$curr) {
        updateSelectInput(session, "anchor_currency", selected = query$currency)
      }
      updateTabsetPanel(session, "main_tabs", selected = "Currency Conversion Model")
    }
  }, ignoreInit = FALSE, once = TRUE)

  observeEvent(input$go_item, {
    if (input$go_item %in% prices()$item_canonical) {
      updateSelectInput(session, "item", selected = input$go_item)
      updateTabsetPanel(session, "main_tabs", selected = "Item View")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$go_store, {
    if (input$go_store %in% prices()$store) {
      updateSelectInput(session, "store", selected = input$go_store)
      updateTabsetPanel(session, "main_tabs", selected = "Store View")
    }
  }, ignoreInit = TRUE)

  store_currency <- reactive({
    first(prices()$curr[prices()$store == input$store])
  })

  output$store_currency_label <- renderText({
    curr <- store_currency()
    if (!length(curr) || is.na(curr)) {
      "Currency"
    } else {
      paste0(curr, " = ", currency_label(curr), " (currency)")
    }
  })

  output$store_rate <- renderText({
    rate <- store_model()$rates %>%
      filter(curr == store_currency()) %>%
      pull(dia_per_currency)
    curr <- store_currency()
    if (!length(rate) || is.na(rate) || !length(curr) || is.na(curr)) {
      "No rate"
    } else {
      paste0("1 ", curr, " = ", fmt_sig(rate, 3), " DIA")
    }
  })

  output$store_count <- renderText({
    n <- store_model()$valued %>%
      filter(store == input$store, !is.na(effective_dia_unit)) %>%
      collapse_visible_store_rows() %>%
      nrow()
    format(n, big.mark = ",")
  })

  output$store_top <- renderText({
    x <- store_model()$valued %>%
      filter(store == input$store, !is.na(direct_value_vs_rate)) %>%
      collapse_visible_store_rows() %>%
      arrange(desc(direct_value_vs_rate), effective_dia_unit, flexibility_rank) %>%
      slice_head(n = 1)
    if (nrow(x) == 0) "No anchor" else x$item
  })

  collapse_visible_store_rows <- function(x) {
    x %>%
      group_by(row_id, store, item, price, curr, limit) %>%
      arrange(flexibility_rank, effective_dia_unit, .by_group = TRUE) %>%
      summarise(
        item_key = first(item_key),
        item_canonical = if_else(n_distinct(item_canonical) > 1, "Multiple choices", first(item_canonical)),
        item_link_target = first(item_canonical),
        qty = first(qty),
        comparable_qty = first(comparable_qty),
        comparable_unit = if_else(n_distinct(comparable_unit) > 1, "choice", first(comparable_unit)),
        anchor_group = first(anchor_group),
        flexibility_rank = min(flexibility_rank, na.rm = TRUE),
        dia_per_currency = first(dia_per_currency),
        direct_dia_unit = min(direct_dia_unit, na.rm = TRUE),
        direct_dia_sources = paste(sort(unique(direct_dia_sources[!is.na(direct_dia_sources)])), collapse = ", "),
        effective_dia_unit = min(effective_dia_unit, na.rm = TRUE),
        direct_value_vs_rate = max(direct_value_vs_rate, na.rm = TRUE),
        best_effective_dia_unit = min(best_effective_dia_unit, na.rm = TRUE),
        best_store = paste(sort(unique(best_store[!is.na(best_store)])), collapse = ", "),
        normal_effective_dia_unit = median(normal_effective_dia_unit, na.rm = TRUE),
        priority_score = max(priority_score, na.rm = TRUE),
        normal_value_ratio = max(normal_value_ratio, na.rm = TRUE),
        total_effective_dia = first(total_effective_dia),
        .groups = "drop"
      ) %>%
      mutate(across(
        c(direct_dia_unit, effective_dia_unit, direct_value_vs_rate, best_effective_dia_unit,
          normal_effective_dia_unit, priority_score, normal_value_ratio),
        ~ if_else(is.infinite(.x), NA_real_, .x)
      ))
  }

  collapse_visible_anchor_rows <- function(x) {
    x %>%
      group_by(row_id, store, item, price, curr, limit) %>%
      arrange(is_excluded, desc(selected_anchor), desc(observed_dia_per_currency), flexibility_rank, .by_group = TRUE) %>%
      summarise(
        item_key = first(item_key),
        qty = first(qty),
        comparable_qty = first(comparable_qty),
        comparable_unit = if_else(n_distinct(comparable_unit) > 1, "choice", first(comparable_unit)),
        direct_dia_sources = paste(sort(unique(direct_dia_sources[direct_dia_sources != "" & !is.na(direct_dia_sources)])), collapse = ", "),
        direct_dia_unit = safe_min(direct_dia_unit),
        observed_dia_per_currency = safe_max(observed_dia_per_currency),
        network_value_ratio = safe_max(network_value_ratio),
        network_weight = safe_max(network_weight),
        is_excluded = all(is_excluded),
        excluded_reason = first(excluded_reason),
        .groups = "drop"
      ) %>%
      mutate(across(
        c(direct_dia_unit, observed_dia_per_currency, network_value_ratio, network_weight),
        ~ if_else(is.infinite(.x), NA_real_, .x)
      ))
  }

  store_rows <- reactive({
    x <- store_model()$valued %>% filter(store == input$store)
    x <- x %>% filter(!is.na(effective_dia_unit))
    x <- collapse_visible_store_rows(x)
    x <- x %>% arrange(desc(normal_value_ratio), effective_dia_unit, flexibility_rank)
    x
  })

  output$store_table <- renderTable({
    price_col <- paste0("Price (", store_currency(), ")")
    dia_col <- dia_unit_col(store_currency())
    store_rows() %>%
      transmute(
        Item = item_link_cell(item, item_key, item_link_target),
        Qty = fmt_num(qty, 0),
        `Base Unit Qty` = base_unit_qty(comparable_qty, comparable_unit),
        !!price_col := fmt_num(price, 0),
        Limit = fmt_limit(limit, store),
        !!dia_col := bold_cell(fmt_num(effective_dia_unit, 2)),
        `Best DIA/unit` = fmt_num(best_effective_dia_unit, 2),
        `Best Store` = store_links_cell(best_store, input$store),
        `Normal DIA/unit` = fmt_num(normal_effective_dia_unit, 2),
        `Value vs normal` = deal_badge(normal_value_ratio)
      ) %>%
      head(80)
  }, sanitize.text.function = identity)

  item_rows <- reactive({
    key <- prices()$item_key[match(input$item, prices()$item_canonical)]
    item_model()$valued %>%
      filter(
        item_key == key |
          (key != "universal speed up hour" & item_key == "universal speed up hour" & str_detect(key, "speed up hour")) |
          (key %in% c("drone component level 1 equivalent", "drone component choice level 1 equivalent") &
             item_key %in% c("drone component level 1 equivalent", "drone component choice level 1 equivalent"))
      ) %>%
      group_by(network_item_key) %>%
      mutate(
        view_best_effective_dia_unit = min(effective_dia_unit, na.rm = TRUE),
        view_normal_effective_dia_unit = first(direct_dia_unit[!is.na(direct_dia_unit)]),
        view_priority_score = if_else(
          !is.na(effective_dia_unit) & is.finite(view_best_effective_dia_unit) & effective_dia_unit > 0,
          100 * view_best_effective_dia_unit / effective_dia_unit,
          NA_real_
        ),
        view_normal_value_ratio = if_else(
          !is.na(effective_dia_unit) & is.finite(view_normal_effective_dia_unit) & effective_dia_unit > 0,
          view_normal_effective_dia_unit / effective_dia_unit,
          NA_real_
        )
      ) %>%
      ungroup() %>%
      arrange(effective_dia_unit, flexibility_rank)
  })

  item_summary <- reactive({
    rows <- item_rows() %>%
      filter(!is.na(effective_dia_unit), is.finite(effective_dia_unit))
    if (nrow(rows) == 0) {
      return(tibble(
        best_dia = NA_real_,
        normal_dia = NA_real_,
        min_dia = NA_real_,
        max_dia = NA_real_,
        best_store = NA_character_
      ))
    }
    best_dia <- min(rows$effective_dia_unit, na.rm = TRUE)
    best_stores <- rows %>%
      filter(abs(effective_dia_unit - best_dia) < 1e-9) %>%
      pull(store) %>%
      unique() %>%
      sort()
    tibble(
      best_dia = best_dia,
      normal_dia = median(rows$view_normal_effective_dia_unit, na.rm = TRUE),
      min_dia = min(rows$effective_dia_unit, na.rm = TRUE),
      max_dia = max(rows$effective_dia_unit, na.rm = TRUE),
      best_store = paste(best_stores, collapse = ", ")
    )
  })

  output$item_best_store <- renderUI({
    stores <- item_summary()$best_store
    if (!length(stores) || is.na(stores) || stores == "") {
      HTML("No store")
    } else {
      HTML(store_links_cell(stores))
    }
  })

  output$item_best_dia <- renderText({
    fmt_sig(item_summary()$best_dia, 3)
  })

  output$item_normal_dia <- renderText({
    fmt_sig(item_summary()$normal_dia, 3)
  })

  output$item_dia_range <- renderText({
    summary <- item_summary()
    if (is.na(summary$min_dia) || is.na(summary$max_dia)) {
      ""
    } else {
      paste0(fmt_sig(summary$min_dia, 3), "-", fmt_sig(summary$max_dia, 3))
    }
  })

  output$item_table <- renderTable({
    item_rows() %>%
      transmute(
        Rank = row_number(),
        Store = store_link_cell(store),
        Item = item_cell(item, item_key),
        Qty = fmt_num(qty, 0),
        `Base Unit Qty` = base_unit_qty(comparable_qty, comparable_unit),
        Price = paste(fmt_num(price, 0), curr),
        Limit = fmt_limit(limit, store),
        `~DIA/unit` = bold_cell(fmt_num(effective_dia_unit, 2)),
        `Normal DIA/unit` = fmt_num(view_normal_effective_dia_unit, 2),
        `Value vs normal` = deal_badge(view_normal_value_ratio)
      )
  }, sanitize.text.function = identity)

  stam_reward_values <- reactive({
    model <- currency_model()
    item_values <- model$direct %>%
      select(item_key, dia_unit = direct_dia_unit)

    dia_for_item <- function(item_key, comparable_qty = 1) {
      value <- item_values$dia_unit[match(item_key, item_values$item_key)]
      ifelse(length(value) == 0 || is.na(value), NA_real_, comparable_qty * value)
    }

    sr_amount <- function(resource) {
      amount <- resource_chest_amounts(input$hq_level) %>%
        filter(resource == !!resource, tier == "sr") %>%
        pull(amount)
      if (!length(amount) || is.na(amount)) {
        amount <- resource_chest_amounts(29) %>%
          filter(resource == !!resource, tier == "sr") %>%
          pull(amount)
      }
      amount
    }

    fmt_stam_qty <- function(x) {
      case_when(
        abs(x) >= 1e6 ~ paste0(fmt_num(x / 1e6, 2), "m"),
        abs(x) >= 1e3 ~ paste0(fmt_num(x / 1e3, 1), "k"),
        TRUE ~ fmt_num(x, 0)
      )
    }

    iron_50k_value <- dia_for_item("iron resource", 50000 / sr_amount("iron"))
    five_min_speed_value <- dia_for_item("universal speed up hour", 1 / 12)
    s1_gift_chest_value <- 0.70 * iron_50k_value + 0.30 * five_min_speed_value
    hero_exp_qty <- 683000
    monica_bonus <- isTRUE(input$stam_monica)
    iron_food_qty <- if (monica_bonus) 478500 else 276000
    coin_qty <- if (monica_bonus) 305100 else 176000

    tibble::tribble(
      ~reward, ~qty_label, ~chance, ~icon_item, ~dia_if_received, ~note,
      "Battle Data", "2.5k", 1, "Battle Data (10k)", dia_for_item("battle data", 2500 / 10000), "",
      "Hero EXP", fmt_stam_qty(hero_exp_qty), 1, "Hero EXP Chest (SSR)", dia_for_item("hero exp chest sr equivalent", hero_exp_qty / sr_amount("hero_exp")), "Converted through SR Hero EXP chest contents for the selected HQ level.",
      "Iron", fmt_stam_qty(iron_food_qty), 1, "Iron", dia_for_item("iron resource", iron_food_qty / sr_amount("iron")), if_else(monica_bonus, "Includes Monica's Treasure Hunter Lvl 26 resource bonus.", ""),
      "Food", fmt_stam_qty(iron_food_qty), 1, "Food", dia_for_item("food resource", iron_food_qty / sr_amount("food")), if_else(monica_bonus, "Includes Monica's Treasure Hunter Lvl 26 resource bonus.", ""),
      "Coins", fmt_stam_qty(coin_qty), 1, "Coins", dia_for_item("coins resource", coin_qty / sr_amount("coins")), if_else(monica_bonus, "Includes Monica's Treasure Hunter Lvl 26 resource bonus.", ""),
      "Diamonds", "20", 1 / 3, "Diamonds", 20, "",
      "Hero Recruitment Ticket", "1", 1 / 3, "Hero Recruitment Ticket", dia_for_item("hero recruitment ticket", 1), "",
      "Gear Chest (SR)", "1", 1 / 3, "Gear Chest (SR)", dia_for_item("superalloy equivalent", 40), "Train-calculator equivalence: 1 SR gear chest = 40 Superalloy.",
      "5-min Speed-Ups", "2", 1 / 3, "5m Speed-Up Chest", dia_for_item("universal speed up hour", 2 / 12), "",
      "Survivor Recruitment Ticket", "1", 1 / 3, "Survivor Recruitment Ticket", dia_for_item("survivor recruitment ticket", 1), "",
      "S1 Gift Chest", "1", 1 / 3, "S1 Gift Chest", s1_gift_chest_value, "Chest EV: 70% 50k iron, 30% 5-min speed-up."
    ) %>%
      mutate(expected_dia = chance * dia_if_received)
  })

  arms_race_bonus_values <- reactive({
    model <- currency_model()
    item_values <- model$direct %>%
      select(item_key, dia_unit = direct_dia_unit)

    dia_for_item <- function(item_key, comparable_qty = 1) {
      value <- item_values$dia_unit[match(item_key, item_values$item_key)]
      ifelse(length(value) == 0 || is.na(value), NA_real_, comparable_qty * value)
    }

    tier_value <- function(skill_medals, sr_chests_each, five_min_speedups) {
      dia_for_item("skill medal", skill_medals) +
        dia_for_item("food resource", sr_chests_each) +
        dia_for_item("iron resource", sr_chests_each) +
        dia_for_item("coins resource", sr_chests_each) +
        dia_for_item("universal speed up hour", five_min_speedups / 12)
    }

    tibble::tibble(
      tier = c("1 chest", "2 chests", "3 chests"),
      stamina = c(15, 100, 300),
      skill_medals = c(200, 600, 700),
      sr_chests_each = c(4, 12, 32),
      five_min_speedups = c(8, 28, 68)
    ) %>%
      rowwise() %>%
      mutate(
        bonus_dia = tier_value(skill_medals, sr_chests_each, five_min_speedups),
        dia_per_stam = bonus_dia / stamina
      ) %>%
      ungroup()
  })

  stam_known_total <- reactive({
    sum(stam_reward_values()$expected_dia, na.rm = TRUE)
  })

  output$stam_bonus_note <- renderUI({
    has_bonus <- isTRUE(input$stam_arms_race) || isTRUE(input$stam_monica)
    notes <- c(if_else(
      has_bonus,
      "Showing Doom Elite rally rewards with selected bonus assumptions.",
      "Showing no-bonus Doom Elite rally rewards."
    ))
    if (isTRUE(input$stam_arms_race)) {
      notes <- c(notes, "Arms Race bonus tiers are shown below as separate cumulative milestone values.")
    }
    if (isTRUE(input$stam_monica)) {
      notes <- c(notes, "Monica's Treasure Hunter Lvl 26 is applied only to the core food, iron, and coin rewards.")
    }
    div(class = "note", paste(notes, collapse = " "))
  })

  output$stam_total_value <- renderText({
    paste0(fmt_sig(stam_known_total(), 3), " DIA")
  })

  output$stam_value_per_stam <- renderText({
    paste0(fmt_num(stam_known_total() / 20, 2), " DIA")
  })

  output$stam_table <- renderTable({
    fmt_dia_or_pending <- function(x) {
      ifelse(is.na(x), "Pending", fmt_sig(x, 3))
    }

    stam_reward_values() %>%
      transmute(
        Reward = item_cell(reward, icon_item),
        Qty = qty_label,
        Chance = paste0(fmt_num(100 * chance, 1), "%"),
        `DIA if received` = fmt_dia_or_pending(dia_if_received),
        `Expected DIA` = fmt_dia_or_pending(expected_dia),
        Note = note
      ) %>%
      bind_rows(tibble(
        Reward = "<strong>Total per Doom Elite rally</strong>",
        Qty = "",
        Chance = "",
        `DIA if received` = "",
        `Expected DIA` = bold_cell(fmt_sig(stam_known_total(), 3)),
        Note = "Uses selected HQ level where available; otherwise uses HQ 29."
      )) %>%
      bind_rows(tibble(
        Reward = "<strong>Total per 1 Stamina</strong>",
        Qty = "",
        Chance = "",
        `DIA if received` = "",
        `Expected DIA` = bold_cell(fmt_num(stam_known_total() / 20, 2)),
        Note = "20 stamina per Doom Elite rally."
      ))
  }, sanitize.text.function = identity)

  output$stam_purchase_table <- renderTable({
    value_100 <- stam_known_total() / 20 * 100
    purchase_cell <- function(x, worth_it) {
      cls <- ifelse(worth_it, "stam-buy-good", "stam-buy-bad")
      paste0("<span class='stam-buy-cell ", cls, "'>", x, "</span>")
    }

    tibble(cost = c(300, 500, 1000)) %>%
      mutate(
        worth_it = value_100 >= cost,
        net = value_100 - cost
      ) %>%
      transmute(
        Purchase = purchase_cell(paste0("Purchase 100 Stamina for ", fmt_num(cost, 0), " Diamonds"), worth_it),
        `Modeled payoff` = purchase_cell(paste0(fmt_sig(value_100, 3), " Diamonds"), worth_it),
        `Net value` = purchase_cell(paste0(ifelse(net >= 0, "+", ""), fmt_sig(net, 3), " Diamonds"), worth_it),
        Decision = purchase_cell(ifelse(worth_it, "Worth buying", "Not worth buying"), worth_it)
      )
  }, sanitize.text.function = identity)

  output$arms_race_bonus_heading <- renderUI({
    if (!isTRUE(input$stam_arms_race)) return(NULL)
    div(
      class = "control-card note",
      strong("Arms Race Bonus Value"),
      tags$br(),
      "Each row is cumulative: 2 chests includes the 15-stamina and 100-stamina bonuses; 3 chests includes all three bonus lines."
    )
  })

  output$arms_race_bonus_table <- renderTable({
    if (!isTRUE(input$stam_arms_race)) return(NULL)

    arms_race_bonus_values() %>%
      transmute(
        Tier = tier,
        `Stamina target` = fmt_num(stamina, 0),
        Rewards = paste0(
          fmt_num(skill_medals, 0), " Skill Medal + ",
          fmt_num(sr_chests_each, 0), " each SR Food/Iron/Coin Chest + ",
          fmt_num(five_min_speedups, 0), " 5-min Speed-Up"
        ),
        `Bonus DIA` = fmt_sig(bonus_dia, 3),
        `Bonus DIA/Stam` = bold_cell(fmt_num(dia_per_stam, 2))
      )
  }, sanitize.text.function = identity)

  train_item_values <- reactive({
    items <- train_items(input$hq_level)
    model <- currency_model()
    item_values <- model$direct %>%
      select(item_key, dia_unit = direct_dia_unit)
    currency_values <- model$rates %>%
      select(curr, dia_per_currency)

    items %>%
      rowwise() %>%
      mutate(
        dia_each = case_when(
          value_kind == "flat" ~ flat_dia,
          value_kind == "currency" ~ comparable_qty * currency_values$dia_per_currency[match(currency, currency_values$curr)],
          value_kind == "item" ~ comparable_qty * item_values$dia_unit[match(item_key, item_values$item_key)],
          TRUE ~ NA_real_
        )
      ) %>%
      ungroup()
  })

  train_slot_input <- function(id) {
    value <- suppressWarnings(as.numeric(input[[paste0("train_", id)]]))
    if (length(value) == 0 || is.na(value)) {
      return(0)
    }
    value
  }

  clear_train_builder <- function(queue = 5) {
    ids <- train_items(input$hq_level)$id
    for (id in ids) {
      updateNumericInput(session, paste0("train_", id), value = 0)
    }
    updateNumericInput(session, "train_queue", value = queue)
    session$sendCustomMessage("updateTrainSlotColors", list())
  }

  load_train_builder <- function(car) {
    clear_train_builder(queue = car$queue)
    if (!is.null(car$counts) && length(car$counts)) {
      for (id in names(car$counts)) {
        updateNumericInput(session, paste0("train_", id), value = car$counts[[id]])
      }
    }
    session$sendCustomMessage("updateTrainSlotColors", list())
  }

  output$train_inputs <- renderUI({
    rows <- train_item_values()
    div(class = "train-grid",
        lapply(seq_len(nrow(rows)), function(i) {
          item <- rows[i, ]
          div(
            class = paste("train-item", paste0("train-item-", item$tier)),
            div(
              class = "train-qty-control train-slot-empty",
              numericInput(
                inputId = paste0("train_", item$id),
                label = HTML(train_item_cell(item$label, item$icon_item)),
                value = 0,
                min = 0,
                max = 6,
                step = 1
              )
            ),
            div(
              class = "train-item-value",
              if (is.na(item$dia_each)) {
                "Value: pending"
              } else {
                paste0("Value: ~", fmt_dia_equiv(item$dia_each), " Diamonds")
              }
            )
          )
        })
    )
  })

  train_rows <- reactive({
    train_item_values() %>%
      rowwise() %>%
      mutate(
        slot_count = train_slot_input(id),
        dia_total = slot_count * dia_each
      ) %>%
      ungroup()
  })

  train_slot_total <- reactive({
    sum(train_rows()$slot_count, na.rm = TRUE)
  })

  train_slot_state <- reactive({
    slots <- train_slot_total()
    case_when(
      slots > 6 ~ "over",
      slots == 6 ~ "ready",
      slots == 0 ~ "empty",
      TRUE ~ "under"
    )
  })

  output$train_slot_count <- renderText({
    fmt_num(train_slot_total(), 0)
  })

  output$train_total_dia <- renderText({
    paste0(fmt_dia_equiv(sum(train_rows()$dia_total, na.rm = TRUE)), " DIA")
  })

  train_queue_n <- reactive({
    q <- suppressWarnings(as.numeric(input$train_queue))
    q <- ifelse(is.na(q), 0, q)
    pmin(pmax(q, 0), 100)
  })

  train_total_value <- reactive({
    sum(train_rows()$dia_total, na.rm = TRUE)
  })

  train_selection_probability <- reactive({
    5 / max(train_queue_n() + 1, 5)
  })

  current_train_snapshot <- reactive({
    rows <- train_rows() %>%
      filter(slot_count > 0) %>%
      arrange(desc(tier == "top"), desc(dia_total), desc(dia_each))

    featured <- if (nrow(rows) > 0) rows[1, ] else NULL
    counts <- rows$slot_count
    names(counts) <- rows$id

    list(
      featured_label = if (is.null(featured)) "" else featured$label,
      featured_icon = if (is.null(featured)) "" else featured$icon_item,
      counts = as.list(counts),
      total_value = train_total_value(),
      queue = train_queue_n(),
      probability = train_selection_probability(),
      ev = train_selection_probability() * train_total_value()
    )
  })

  output$train_save_control <- renderUI({
    saved <- saved_train_cars()
    edit_index <- editing_train_car()
    slots <- train_slot_total()
    can_save <- slots == 6 && (!is.null(edit_index) || length(saved) < 4)
    save_note <- case_when(
      !is.null(edit_index) && slots == 6 ~ paste0("Ready to update Train Car ", edit_index, "."),
      !is.null(edit_index) ~ paste0("Editing Train Car ", edit_index, ". Load exactly 6 items to update."),
      length(saved) >= 4 ~ "Comparison is full. Clear saved cars to start over.",
      slots == 6 ~ "Ready to save this train car.",
      TRUE ~ paste0("Load exactly 6 items to save. Current total: ", fmt_num(slots, 0), ".")
    )

    save_label <- if (is.null(edit_index)) "Save Train Car" else paste0("Update Train Car ", edit_index)
    save_args <- list(inputId = "save_train_car", label = save_label, class = "btn-primary")
    if (!can_save) save_args$disabled <- "disabled"

    clear_args <- list(inputId = "clear_train_cars", label = "Clear All")
    if (length(saved) == 0) clear_args$disabled <- "disabled"

    div(
      class = "train-save-row",
      do.call(actionButton, save_args),
      do.call(actionButton, clear_args),
      span(class = "note", save_note)
    )
  })

  observeEvent(input$save_train_car, {
    if (train_slot_total() != 6) return()
    saved <- saved_train_cars()
    edit_index <- editing_train_car()
    if (is.null(edit_index) && length(saved) >= 4) return()
    snapshot <- current_train_snapshot()
    if (!is.null(edit_index) && edit_index >= 1 && edit_index <= length(saved)) {
      snapshot$name <- saved[[edit_index]]$name
      saved[[edit_index]] <- snapshot
      saved_train_cars(saved)
    } else {
      snapshot$name <- paste0("Train Car ", length(saved) + 1)
      saved_train_cars(append(saved, list(snapshot)))
    }
    editing_train_car(NULL)
    clear_train_builder()
  })

  observeEvent(input$clear_train_cars, {
    saved_train_cars(list())
    editing_train_car(NULL)
    clear_train_builder()
  })

  observeEvent(input$clear_train_car, {
    idx <- suppressWarnings(as.integer(input$clear_train_car))
    saved <- saved_train_cars()
    if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > length(saved)) return()
    saved[[idx]] <- NULL
    if (length(saved)) {
      for (i in seq_along(saved)) {
        saved[[i]]$name <- paste0("Train Car ", i)
      }
    }
    current_edit <- editing_train_car()
    if (!is.null(current_edit)) {
      if (current_edit == idx) {
        editing_train_car(NULL)
        clear_train_builder()
      } else if (current_edit > idx) {
        editing_train_car(current_edit - 1)
      }
    }
    saved_train_cars(saved)
  })

  observeEvent(input$edit_train_car, {
    idx <- suppressWarnings(as.integer(input$edit_train_car))
    saved <- saved_train_cars()
    if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > length(saved)) return()
    editing_train_car(idx)
    load_train_builder(saved[[idx]])
  })

  output$saved_train_cars <- renderUI({
    saved <- saved_train_cars()
    cards <- lapply(seq_len(4), function(i) {
      if (i > length(saved)) {
        return(div(class = "saved-train-card saved-train-placeholder", paste0("Train Car ", i)))
      }
      car <- saved[[i]]
      div(
        class = "saved-train-card",
        div(class = "saved-title", htmltools::htmlEscape(car$name)),
        div(class = "saved-row saved-row-featured",
            span(class = "saved-label", "Featured item"),
            span(class = "saved-value", HTML(train_item_cell(car$featured_label, car$featured_icon)))),
        div(class = "saved-row",
            span(class = "saved-label", "Total Item Value"),
            span(class = "saved-value", paste0(fmt_dia_compact(car$total_value), " Diamonds"))),
        div(class = "saved-row",
            span(class = "saved-label", "Queue Length"),
            span(class = "saved-value", fmt_num(car$queue, 0))),
        div(class = "saved-row",
            span(class = "saved-label", "Join EV"),
            span(class = "saved-value", paste0(fmt_dia_compact(car$ev), " Diamonds"))),
        div(
          class = "saved-train-actions",
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs",
            onclick = sprintf("Shiny.setInputValue('edit_train_car', %s, {priority: 'event'});", i),
            "Edit"
          ),
          tags$span(" "),
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs",
            onclick = sprintf("Shiny.setInputValue('clear_train_car', %s, {priority: 'event'});", i),
            "Clear"
          )
        )
      )
    })
    div(
      class = "saved-train-panel",
      h4("Saved Train Cars"),
      div(class = "saved-train-grid", cards)
    )
  })

  output$train_selection_probability <- renderText({
    paste0(fmt_num(100 * train_selection_probability(), 1), "%")
  })

  output$train_queue_ev <- renderText({
    paste0(fmt_dia_equiv(train_selection_probability() * train_total_value()), " DIA")
  })

  output$train_warning <- renderUI({
    slots <- train_slot_total()
    if (slots == 6) {
      div(class = "train-warning train-warning-ok", "Ready: this train car has exactly 6 item slots.")
    } else if (slots > 6) {
      div(class = "train-warning train-warning-over", paste0("Too many item slots. Train cars need exactly 6; current total: ", fmt_num(slots, 0), "."))
    } else {
      div(class = "train-warning train-warning-bad", paste0("Train cars need exactly 6 item slots. Current total: ", fmt_num(slots, 0), "."))
    }
  })

  output$train_table <- renderTable({
    rows <- train_rows() %>%
      filter(slot_count > 0)
    if (nrow(rows) == 0) {
      return(tibble(Message = "Enter item counts below to calculate this train car."))
    }

    rows %>%
      transmute(
        Item = train_summary_cell(train_item_cell(label, icon_item), tier),
        Count = train_summary_cell(fmt_num(slot_count, 0), tier),
        `Diamond Value Each` = train_summary_cell(fmt_dia_equiv(dia_each), tier),
        `Total Diamond Value` = train_summary_cell(bold_cell(fmt_dia_equiv(dia_total)), tier)
      ) %>%
      bind_rows(tibble(
        Item = "<strong>Total Train Car Value</strong>",
        Count = "",
        `Diamond Value Each` = "",
        `Total Diamond Value` = bold_cell(fmt_dia_equiv(train_total_value()))
      )) %>%
      bind_rows(tibble(
        Item = "<strong>Selection Probability</strong>",
        Count = "",
        `Diamond Value Each` = "",
        `Total Diamond Value` = bold_cell(paste0(fmt_num(100 * train_selection_probability(), 1), "%"))
      )) %>%
      bind_rows(tibble(
        Item = "<strong>Queue Position EV</strong>",
        Count = "",
        `Diamond Value Each` = "",
        `Total Diamond Value` = bold_cell(paste0(fmt_dia_equiv(train_selection_probability() * train_total_value()), " DIA"))
      ))
  }, sanitize.text.function = identity)

  output$rates_table <- renderTable({
    currency_model()$rates %>%
      arrange(dia_per_currency) %>%
      transmute(
        View = currency_anchor_link(curr),
        Currency = currency_cell(curr),
        `Network DIA/curr` = fmt_sig(dia_per_currency, 2),
        `Connected items` = if_else(is.na(anchor_items), "", as.character(anchor_items)),
        `Observed min` = fmt_sig(min_anchor, 2),
        `Observed median` = fmt_sig(median_anchor, 2),
        `Observed max` = fmt_sig(max_anchor, 2)
      )
  }, sanitize.text.function = identity)

  output$network_table <- renderTable({
    currency_model()$diagnostics %>%
      filter(direction != "Near network") %>%
      group_by(row_id, store, item, qty, price, curr, limit, direction) %>%
      summarise(
        item_key = first(item_key),
        network_value_ratio = safe_max(network_value_ratio),
        .groups = "drop"
      ) %>%
      arrange(desc(abs(log(network_value_ratio)))) %>%
      transmute(
        Direction = direction,
        Store = store_link_cell(store),
        Item = item_cell(item, item_key),
        Price = paste(fmt_num(price, 0), curr),
        Limit = fmt_limit(limit, store),
        `Value vs network` = deal_badge(network_value_ratio)
      ) %>%
      head(12)
  }, sanitize.text.function = identity)

  output$anchors_table <- renderTable({
    anchors <- currency_model()$anchor_candidates
    if (!is.null(input$anchor_currency) && input$anchor_currency != "__ALL__") {
      anchors <- anchors %>% filter(curr == input$anchor_currency)
    }
    anchors %>%
      collapse_visible_anchor_rows() %>%
      arrange(curr, desc(observed_dia_per_currency)) %>%
      transmute(
        Store = excluded_cell(store, is_excluded),
        Item = excluded_cell(item_cell(item, item_key), is_excluded),
        Qty = excluded_cell(fmt_num(qty, 0), is_excluded),
        `Base Unit Qty` = excluded_cell(base_unit_qty(comparable_qty, comparable_unit), is_excluded),
        Price = excluded_cell(paste(fmt_num(price, 0), curr), is_excluded),
        Limit = excluded_cell(fmt_limit(limit, store), is_excluded),
        `Model source` = excluded_cell(direct_dia_sources, is_excluded),
        `Network DIA / base unit` = excluded_cell(fmt_num(direct_dia_unit, 2), is_excluded),
        `DIA/curr` = excluded_cell(fmt_num(observed_dia_per_currency, 2), is_excluded),
        `Value vs network` = excluded_cell(deal_badge(network_value_ratio), is_excluded),
        Weight = excluded_cell(fmt_num(network_weight, 2), is_excluded)
      ) %>%
      head(400)
  }, sanitize.text.function = identity)

  output$raw_table <- renderTable({
    prices() %>%
      transmute(
        Store = store,
        Item = item_cell(item, item_key),
        Qty = fmt_num(qty, 0),
        `Base Unit Qty` = base_unit_qty(comparable_qty, comparable_unit),
        Price = paste(fmt_num(price, 0), curr),
        Limit = fmt_limit(limit, store),
        `Compared As` = item_canonical
      ) %>%
      head(300)
  }, sanitize.text.function = identity)
}

shinyApp(ui, server)

