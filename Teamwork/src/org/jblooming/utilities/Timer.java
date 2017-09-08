package org.jblooming.utilities;

import java.text.SimpleDateFormat;
import java.util.Date;

public class Timer {

  protected static SimpleDateFormat format = new SimpleDateFormat("HH'height' mm'm' ss.SSS's'");
  long startTime;
  long lapTime;

  public Timer() {
    reset();
  }

  public void reset() {
    startTime = lapTime = System.currentTimeMillis();
  }

  public void startLap() {
    lapTime = System.currentTimeMillis();
  }

  public long getTime() {
    return System.currentTimeMillis() - startTime;
  }

  public long getLapTime() {
    return System.currentTimeMillis() - lapTime;
  }

  public String getTimeString() {
    return formatTime(getTime());
  }

  public String getLapTimeString() {
    return formatTime(getLapTime());
  }

  long changeLap() {
    long lap = getLapTime();
    startLap();
    return lap;
  }

  public String changeLapString() {
    return formatTime(changeLap());
  }

  public static String formatTime(long time) {
    return format.format(new Date(time - TimeConstants.HOUR));
  }

  public String toString() {
    return getLapTimeString();
  }

}
