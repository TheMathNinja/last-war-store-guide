# Last War Store Guide Shiny App

This app reads `C:/Users/Michael/Desktop/Last War Price Guide.xlsx`, normalizes the store blocks into one tidy listing table, and converts all store currencies into diamond-equivalent values.

## How the conversion works

- Diamonds (`DIA`) are the universal currency.
- Direct diamond prices establish the anchor value for each matching item.
- Item labels are normalized into comparable value units before prices are compared:
  - speed-ups are priced per 1 hour
  - resource chests and resource choice chests are priced per underlying resource amount
  - Battle Data (10K) is treated as 10,000 Battle Data
  - drone component levels use 3 lower-level components per next level
  - Superalloy, Synthetic Resin, and Dielectric Ceramic use a 4x crafting ladder
  - Hero EXP chests use SR-equivalent units, with SSR = 8x SR and UR = 3x SSR
- Each non-diamond currency is inferred from overlapping items:
  - `observed DIA per currency = best direct diamond unit price / native currency unit price`
  - the app uses an adjustable exchange-rate strictness percentile, defaulting to 80, so weak deals do not drag down the conversion. 50 uses the middle-ranked overlap; 100 uses only the best observed deal.
- Store and item rankings then use effective diamonds per unit.

Resource chest amounts currently have confirmed HQ 29 values. The app includes an HQ Level slider from 20 to 30 and falls back to HQ 29 quantities for levels that have not been entered yet.

## Icons

Some item icons are bundled from Last War community wiki item pages. Missing icons fall back to compact text badges until clean source art is added.

## Views

- **Store View:** pick a store and rank the best uses of that store's currency.
- **Item View:** pick an item and compare its effective diamond price across stores.
- **Currency Model:** inspect the inferred exchange rates and the anchor observations behind them.
- **Raw Listings:** see the normalized workbook rows and matched item names.
