package org.jblooming.ontology;

import org.hibernate.Hibernate;
import org.hibernate.engine.jdbc.StreamUtils;
import org.hibernate.internal.SessionImpl;
import org.jblooming.PlatformRuntimeException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.sql.Blob;
import java.sql.SQLException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class BinaryLargeObject extends HideableIdentifiableSupport {

  private String originalFileName;
  private int referralId;
  private String referralClass;
  private Blob blob;

  /**
   * Notice that is supports also updates of content.
   *
   * @param inputStream
   */
  public void feed(InputStream inputStream, SessionImpl session) {

    if (inputStream == null)
      throw new PlatformRuntimeException("BinaryLargeObject.add inputStream is null");

    try {
      ByteArrayOutputStream buffer = new ByteArrayOutputStream(inputStream.available());
      StreamUtils.copy(inputStream, buffer);
      blob = session.getLobHelper().createBlob(buffer.toByteArray());

    } catch (IOException e) {
      throw new PlatformRuntimeException(e);
    }

  }

  public InputStream getInputStream() throws SQLException {
    return blob.getBinaryStream();
  }

  public String getOriginalFileName() {
    return originalFileName;
  }

  public void setOriginalFileName(String originalFileName) {
    this.originalFileName = originalFileName;
  }

  public String getName() {
    return originalFileName;
  }

  public int getReferralId() {
    return referralId;
  }

  public void setReferralId(int referralId) {
    this.referralId = referralId;
  }

  public String getReferralClass() {
    return referralClass;
  }

  public void setReferralClass(String referralClass) {
    this.referralClass = referralClass;
  }

  public void setReferral(Identifiable i) {
    if (i!=null) {
      this.referralId = i.getIntId();
      this.referralClass = i.getClass().getName();
    }
  }

  public Blob getBlob() {
    return blob;
  }

  public void setBlob(Blob blob) {
    this.blob = blob;
  }
}
