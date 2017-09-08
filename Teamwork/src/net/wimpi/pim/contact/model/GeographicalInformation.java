/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.model;

import java.io.Serializable;
import java.util.TimeZone;

/**
 * An interface modeling a container for
 * geographical information based on the
 * types and information of the vCard Mime directory
 * profile standard specification.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.4 Geographical Types<br>
 * 3.4.1 TZ Type Definition<br>
 * 3.4.2 GEO Type Definition<br>
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface GeographicalInformation
    extends Serializable {

  /**
   * Returns the time zone information.
   *
   * @return the time zone as <tt>TimeZone</tt>.
   */
  public TimeZone getTimeZone();

  /**
   * Sets the time zone informations.
   *
   * @param timezone the time zone information as
   *        <tt>TimeZone</tt>.
   */
  public void setTimeZone(TimeZone timezone);

  /**
   * Returns the longitude of the geographical
   * position.<br>
   * The value should reflect the decimal value calculated
   * from following formula:
   * decimal = degrees + minutes/60 + seconds/3600
   *
   * @return the longitude as <tt>double</tt>.
   */
  public double getLongitude();

  /**
   * Sets the longitude of the geographical
   * position.<br>
   * The value should reflect the decimal value calculated
   * from following formula:
   * decimal = degrees + minutes/60 + seconds/3600
   *
   * @param longitude the longitude as <tt>double</tt>.
   */
  public void setLongitude(double longitude);

  /**
   * Returns the latitude of the geographical
   * position.<br>
   * The value should reflect the decimal value calculated
   * from following formula:
   * decimal = degrees + minutes/60 + seconds/3600
   *
   * @return the latitude as <tt>double</tt>.
   */
  public double getLatitude();

  /**
   * Sets the latitude of the geographical
   * position.<br>
   * The value should reflect the decimal value calculated
   * from following formula:
   * decimal = degrees + minutes/60 + seconds/3600
   *
   * @param latitude the latitude as <tt>double</tt>.
   */
  public void setLatitude(double latitude);

}//interface GeographicalInformation
