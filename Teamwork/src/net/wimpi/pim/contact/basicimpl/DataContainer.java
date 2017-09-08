/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Image;
import net.wimpi.pim.contact.model.Key;
import net.wimpi.pim.contact.model.Sound;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * A basic, simple and generic implementation of
 * {@link net.wimpi.pim.contact.model.Key},
 * {@link net.wimpi.pim.contact.model.Sound}
 * and {@link net.wimpi.pim.contact.model.Image}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class DataContainer
    implements Key, Sound, Image {

  static final long serialVersionUID = -114960534115119898L;

  //Attributes
  protected String m_Uri;
  protected String m_ContentType;
  protected byte[] m_Data = null;
  protected boolean m_isUri;

  public boolean isData() {
    return !m_isUri;
  }//isData

  public String getContentType() {
    return m_ContentType;
  }//getContentType

  public void setContentType(String ctype) {
    m_ContentType = ctype;
  }//setContentType

  public void setData(byte[] data) {
    m_Data = data;
    m_isUri = false;
  }//setData(String)

  public void setData(String uri)
      throws IOException, MalformedURLException {
    InputStream in = new URL(uri).openStream();
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    byte[] buffer = new byte[2048];
    int amount = 0;
    while ((amount = in.read(buffer)) >= 0) {
      out.write(buffer, 0, amount);
    }
    setData(out.toByteArray());
  }//setData

  public byte[] getData() {
    return m_Data;
  }//getData

  public String getURI() {
    return m_Uri;
  }//getURI

  public void setURI(String uri) {
    m_Uri = uri;
    m_isUri = true;
  }//setURI

  public boolean isURI() {
    return m_isUri;
  }//isURI

  public InputStream getInputStream()
      throws IOException, MalformedURLException {
    if (!m_isUri) {
      return new ByteArrayInputStream(m_Data);
    } else {
      return new URL(m_Uri).openStream();
    }
  }//getInputStream

  public byte[] getDataFromURI()
      throws IOException, MalformedURLException {
    InputStream in = getInputStream();
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    byte[] buffer = new byte[2048];
    int amount = 0;
    while ((amount = in.read(buffer)) >= 0) {
      out.write(buffer, 0, amount);
    }
    return out.toByteArray();
  }//getDataFromURI

}//class DataContainer
