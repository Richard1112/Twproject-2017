/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.GeographicalInformation;

import java.util.TimeZone;


/**
 * A basic and simple implementation of the
 * {@link net.wimpi.pim.contact.model.GeographicalInformation}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class GeographicalInformationImpl
    implements GeographicalInformation {

  static final long serialVersionUID = 3662516535101194217L;

  //instance attributes
  protected TimeZone m_TZ = TimeZone.getDefault();
  protected double m_Longitude;
  protected double m_Latitude;

  public TimeZone getTimeZone() {
    return m_TZ;
  }//getTimeZone

  public void setTimeZone(TimeZone timezone) {
    m_TZ = timezone;
  }//setTimeZone

  public double getLongitude() {
    return m_Longitude;
  }//getLongitude

  public void setLongitude(double longitude) {
    m_Longitude = longitude;
  }//setLongitude

  public double getLatitude() {
    return m_Latitude;
  }//getLatitude

  public void setLatitude(double latitude) {
    m_Latitude = latitude;
  }//setLatitude

}//class GeographicalInformationImpl
