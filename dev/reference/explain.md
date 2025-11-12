# Explain details of a tbl

This is a generic function which gives more details about an object than
[`print()`](https://rdrr.io/r/base/print.html), and is more focused on
human readable output than [`str()`](https://rdrr.io/r/utils/str.html).

## Usage

``` r
explain(x, ...)

show_query(x, ...)
```

## Arguments

- x:

  An object to explain

- ...:

  Other parameters possibly used by generic

## Value

The first argument, invisibly.

## Databases

Explaining a `tbl_sql` will run the SQL `EXPLAIN` command which will
describe the query plan. This requires a little bit of knowledge about
how `EXPLAIN` works for your database, but is very useful for diagnosing
performance problems.

## Examples

``` r
# \donttest{
lahman_s <- dbplyr::lahman_sqlite()
#> Creating table: AllstarFull
#> Creating table: Appearances
#> Creating table: AwardsManagers
#> Creating table: AwardsPlayers
#> Creating table: AwardsShareManagers
#> Creating table: AwardsSharePlayers
#> Creating table: Batting
#> Creating table: BattingPost
#> Creating table: CollegePlaying
#> Creating table: Fielding
#> Creating table: FieldingOF
#> Creating table: FieldingOFsplit
#> Creating table: FieldingPost
#> Creating table: HallOfFame
#> Creating table: HomeGames
#> Creating table: LahmanData
#> Creating table: Managers
#> Creating table: ManagersHalf
#> Creating table: Parks
#> Creating table: People
#> Creating table: Pitching
#> Creating table: PitchingPost
#> Creating table: Salaries
#> Creating table: Schools
#> Creating table: SeriesPost
#> Creating table: Teams
#> Creating table: TeamsFranchises
#> Creating table: TeamsHalf
batting <- tbl(lahman_s, "Batting")
batting |> show_query()
#> <SQL>
#> SELECT *
#> FROM `Batting`
batting |> explain()
#> <SQL>
#> SELECT *
#> FROM `Batting`
#> 
#> <PLAN>
#>   id parent notused       detail
#> 1  2      0     182 SCAN Batting

# The batting database has indices on all ID variables:
# SQLite automatically picks the most restrictive index
batting |> filter(lgID == "NL" & yearID == 2000L) |> explain()
#> <SQL>
#> SELECT `Batting`.*
#> FROM `Batting`
#> WHERE (`lgID` = 'NL' AND `yearID` = 2000)
#> 
#> <PLAN>
#>   id parent notused
#> 1  3      0     125
#>                                                 detail
#> 1 SEARCH Batting USING INDEX Batting_yearID (yearID=?)

# OR's will use multiple indexes
batting |> filter(lgID == "NL" | yearID == 2000) |> explain()
#> <SQL>
#> SELECT `Batting`.*
#> FROM `Batting`
#> WHERE (`lgID` = 'NL' OR `yearID` = 2000.0)
#> 
#> <PLAN>
#>   id parent notused
#> 1  4      0       0
#> 2  5      4       0
#> 3 11      5     181
#> 4 16      4       0
#> 5 22     16     125
#>                                                 detail
#> 1                                       MULTI-INDEX OR
#> 2                                              INDEX 1
#> 3     SEARCH Batting USING INDEX Batting_lgID (lgID=?)
#> 4                                              INDEX 2
#> 5 SEARCH Batting USING INDEX Batting_yearID (yearID=?)

# Joins will use indexes in both tables
teams <- tbl(lahman_s, "Teams")
batting |> left_join(teams, c("yearID", "teamID")) |> explain()
#> <SQL>
#> SELECT
#>   `playerID`,
#>   `Batting`.`yearID` AS `yearID`,
#>   `stint`,
#>   `Batting`.`teamID` AS `teamID`,
#>   `Batting`.`lgID` AS `lgID.x`,
#>   `Batting`.`G` AS `G.x`,
#>   `Batting`.`AB` AS `AB.x`,
#>   `Batting`.`R` AS `R.x`,
#>   `Batting`.`H` AS `H.x`,
#>   `Batting`.`X2B` AS `X2B.x`,
#>   `Batting`.`X3B` AS `X3B.x`,
#>   `Batting`.`HR` AS `HR.x`,
#>   `RBI`,
#>   `Batting`.`SB` AS `SB.x`,
#>   `Batting`.`CS` AS `CS.x`,
#>   `Batting`.`BB` AS `BB.x`,
#>   `Batting`.`SO` AS `SO.x`,
#>   `IBB`,
#>   `Batting`.`HBP` AS `HBP.x`,
#>   `SH`,
#>   `Batting`.`SF` AS `SF.x`,
#>   `GIDP`,
#>   `Teams`.`lgID` AS `lgID.y`,
#>   `franchID`,
#>   `divID`,
#>   `Rank`,
#>   `Teams`.`G` AS `G.y`,
#>   `Ghome`,
#>   `W`,
#>   `L`,
#>   `DivWin`,
#>   `WCWin`,
#>   `LgWin`,
#>   `WSWin`,
#>   `Teams`.`R` AS `R.y`,
#>   `Teams`.`AB` AS `AB.y`,
#>   `Teams`.`H` AS `H.y`,
#>   `Teams`.`X2B` AS `X2B.y`,
#>   `Teams`.`X3B` AS `X3B.y`,
#>   `Teams`.`HR` AS `HR.y`,
#>   `Teams`.`BB` AS `BB.y`,
#>   `Teams`.`SO` AS `SO.y`,
#>   `Teams`.`SB` AS `SB.y`,
#>   `Teams`.`CS` AS `CS.y`,
#>   `Teams`.`HBP` AS `HBP.y`,
#>   `Teams`.`SF` AS `SF.y`,
#>   `RA`,
#>   `ER`,
#>   `ERA`,
#>   `CG`,
#>   `SHO`,
#>   `SV`,
#>   `IPouts`,
#>   `HA`,
#>   `HRA`,
#>   `BBA`,
#>   `SOA`,
#>   `E`,
#>   `DP`,
#>   `FP`,
#>   `name`,
#>   `park`,
#>   `attendance`,
#>   `BPF`,
#>   `PPF`,
#>   `teamIDBR`,
#>   `teamIDlahman45`,
#>   `teamIDretro`
#> FROM `Batting`
#> LEFT JOIN `Teams`
#>   ON (
#>     `Batting`.`yearID` = `Teams`.`yearID` AND
#>     `Batting`.`teamID` = `Teams`.`teamID`
#>   )
#> 
#> <PLAN>
#>   id parent notused
#> 1  4      0     182
#> 2  6      0      67
#>                                                       detail
#> 1                                               SCAN Batting
#> 2 SEARCH Teams USING INDEX Teams_yearID (yearID=?) LEFT-JOIN
# }
```
