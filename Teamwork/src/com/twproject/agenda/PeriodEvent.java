package com.twproject.agenda;

import org.jblooming.agenda.Period;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 11-mag-2007 : 17.39.54
 */
public class PeriodEvent implements Comparable{
  public Period period;
  public Event event;

  public PeriodEvent (Period period, Event event){
    this.event=event;
    this.period=period;
  }


  public int compareTo(Object o) {
    return this.period.compareTo(((PeriodEvent)o).period);
  }
}
