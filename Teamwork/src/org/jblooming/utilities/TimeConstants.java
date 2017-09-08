package org.jblooming.utilities;

public interface TimeConstants {
  public long SECOND = 1000L;
  public long MINUTE = SECOND * 60L;
  public long HOUR = MINUTE * 60L;
  public long DAY = HOUR * 24L;
  public long WEEK = DAY * 7L;
  public long NORMAL_YEAR = DAY * 365L;
  public long LEAP_YEAR = DAY * 366L;
}
