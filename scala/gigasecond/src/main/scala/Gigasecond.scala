import java.time.LocalDate
import java.time.LocalDateTime
import java.time.Duration
import scala.math.pow

object Gigasecond {
  val GIGASECOND = Duration.ofSeconds(pow(10, 9).toInt)
  def add(startDate: LocalDate): LocalDateTime = startDate.atTime(0,0).plus(GIGASECOND)

  def add(startDateTime: LocalDateTime): LocalDateTime = startDateTime.plus(GIGASECOND)
}
