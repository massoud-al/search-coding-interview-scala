package albums.challenge

import albums.challenge.SearchService.{
  MinusOperator,
  OperatorsRegex,
  PlusOperator,
  PriceBracketSize,
  RangeIndicator,
}
import albums.challenge.models.{Entry, Facet, Results}
import org.springframework.stereotype.Service

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

@Service
class SearchService {

  private case class FilterResult(
      value: Entry,
      priceBracket: Integer,
      year: String,
      priceMatch: Boolean,
      yearMatch: Boolean,
  )

  def search(
      entries: List[Entry],
      query: String,
      year: List[String] = List.empty,
      price: List[String] = List.empty,
  ): Results = {
    val filteredEntries: List[FilterResult] = applyFilters(entries, query, year, price)
    val yearFacets: List[Facet] = generateYearFacets(filteredEntries)
    val priceFacets: List[Facet] = generatePriceFacets(filteredEntries)

    val finalResults =
      filteredEntries.filter(entry => entry.priceMatch && entry.yearMatch).map(_.value)
    val facets = Map(("year", yearFacets), ("price", priceFacets))

    Results(
      finalResults,
      facets,
      query,
    )
  }
  private def generatePriceFacets(filteredEntries: List[FilterResult]): List[Facet] = {
    val priceFacets = filteredEntries
      .filter(_.yearMatch)
      .groupMapReduce(_.priceBracket)(_ => 1)(_ + _)
      .toList
      .sortWith(_._1 < _._1)
      .map(countPerRange => {
        val startValue = countPerRange._1 * PriceBracketSize
        val endValue = (countPerRange._1 + 1) * PriceBracketSize
        Facet(s"$startValue $RangeIndicator $endValue", countPerRange._2)
      })
    priceFacets
  }

  private def generateYearFacets(filteredEntries: List[FilterResult]): List[Facet] = {
    val yearFacets = filteredEntries
      .filter(_.priceMatch)
      .groupMapReduce(result => result.year)(_ => 1)(_ + _)
      .toList
      .sortWith(_._1 > _._1)
      .map(countPerYear => Facet(countPerYear._1, countPerYear._2))
    yearFacets
  }

  private def applyFilters(
      entries: List[Entry],
      query: String,
      year: List[String],
      price: List[String],
  ): List[FilterResult] = {
    val priceBrackets = parsePriceBrackets(price)
    val yearFilter = year.toSet

    val filteredEntries = resolveQuery(entries, query).map(entry => {
      val year = parseYear(entry.releaseDate)
      val priceBracket = entry.price.intValue / PriceBracketSize
      FilterResult(
        entry,
        priceBracket,
        year,
        (priceBrackets.isEmpty || priceBrackets.contains(priceBracket)),
        (yearFilter.isEmpty || yearFilter.contains(year)),
      )
    })
    filteredEntries
  }

  private def parsePriceBrackets(price: List[String]): Set[Int] = {
    price
      .collect {
        case range if range.contains('-') => range.split('-').headOption.flatMap(_.trim.toIntOption)
      }
      .filter(_.nonEmpty)
      .map(_.get / PriceBracketSize)
      .toSet
  }

  private def parseYear(date: String): String = {
    Try(LocalDate.parse(date, DateTimeFormatter.ISO_DATE_TIME).getYear.toString)
      .getOrElse("Unknown")
  }

  private def resolveQuery(entries: List[Entry], query: String): List[Entry] = {
    query match {
      case s if s == null || s.isEmpty => entries
      case _                           => applyQuery(entries, query.toLowerCase)
    }
  }

  private def applyQuery(entries: List[Entry], query: String): List[Entry] = {

    if (query.contains(PlusOperator) || query.contains(MinusOperator)) {
      var queryWithOperator: String = query
      if (!query.charAt(0).equals(PlusOperator) && !query.charAt(0).equals(MinusOperator))
        queryWithOperator = PlusOperator + query
      val queriesWithOperator = OperatorsRegex.findAllIn(queryWithOperator).toList
      val queryTerms = queriesWithOperator.groupMap(query => query.charAt(0))(query =>
        query.substring(1).trim.toLowerCase,
      )
      val includeTerms = queryTerms.getOrElse(PlusOperator, List.empty).toSet
      val excludeTerms = queryTerms.getOrElse(MinusOperator, List.empty).toSet
      entries.filter(entry => {
        val title = entry.title.toLowerCase
        (excludeTerms.forall(term => !title.contains(term)) && includeTerms.forall(term =>
          title.contains(term),
        )) || title.contains(query)
      })
    } else {
      entries.filter(entry => entry.title.toLowerCase.contains(query))
    };
  }
}

object SearchService {
  private val PriceBracketSize = 5
  private val RangeIndicator = '-'
  private val OperatorsRegex = """([+-][^+-]*)""".r
  private val MinusOperator = '-'
  private val PlusOperator = '+'
}
