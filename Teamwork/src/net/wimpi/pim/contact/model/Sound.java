/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.model;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.net.MalformedURLException;


/**
 * An interface modeling a sound based on the
 * types and information of the vCard Mime directory
 * profile standard specification.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.6.6 SOUND Type Definition<br>
 * <br>
 * Note that an Image might be <b>either</b> inlined as encoded
 * binary data (MimeBase64 Encoding) <b>or</b> represented
 * by an URI reference. <br>
 * Should refer to or contain data of an IANA registered
 * sound format for maximum compatibility.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface Sound
    extends Serializable {

  /**
   * Tests if this <tt>Sound</tt> is given as
   * an URI reference.
   *
   * @return true if given as URI, false otherwise.
   */
  public boolean isURI();

  /**
   * Returns the URI reference representing
   * this <tt>Sound</tt>.
   *
   * @return the URI reference as <tt>String</tt>.
   */
  public String getURI();

  /**
   * Sets the URI reference representing
   * this <tt>Sound</tt>.
   *
   * @param uri the URI reference as <tt>String</tt>.
   */
  public void setURI(String uri);

  /**
   * Tests if this <tt>Sound</tt> is given as
   * binary data.
   *
   * @return true if given as binary data, false otherwise.
   */
  public boolean isData();

  /**
   * Returns the content type of this <tt>Sound</tt>.
   *
   * @return the content type as <tt>String</tt>.
   */
  public String getContentType();

  /**
   * Sets the content type of this <tt>Sound</tt>.
   *
   * @param ctype the content type as <tt>String</tt>.
   */
  public void setContentType(String ctype);

  /**
   * Returns the non-encoded binary data representing
   * this <tt>Sound</tt>.
   *
   * @return the non-encoded binary data as <tt>byte[]</tt>.
   */
  public byte[] getData();

  /**
   * Sets the non-encoded binary data representing
   * this <tt>Sound</tt>.
   *
   *
   * @param data the non-encoded binary data as <tt>byte[]</tt>.
   */
  public void setData(byte[] data);

  /**
   * Sets the non-encoded binary data as
   * obtained from the given uri.
   * <p>
   * @param uri an URI as <tt>String</tt>.
   */
  public void setData(String uri)
      throws IOException, MalformedURLException;

  /**
   * Returns an <tt>InputStream</tt> providing
   * access to the data of this <tt>Sound</tt>.
   * <p>
   * An implementation might use a
   * <tt>ByteArrayInputStream</tt> for inlined data,
   * or return the input stream which can be obtained
   * from the URI (i.e. using <tt>java.net.URL</tt>).
   *
   * @return an <tt>InputStream</tt> instance.
   */
  public InputStream getInputStream()
      throws IOException, MalformedURLException;

  /**
   * Returns the data obtained from the uri of
   * this <tt>Sound</tt>.
   * <p>
   * @return a <tt>byte[]</tt>.
   */
  public byte[] getDataFromURI()
      throws IOException, MalformedURLException;

}//interface Sound
