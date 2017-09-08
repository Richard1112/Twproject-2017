package com.opnlb.website.news;

import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.SecuredLoggableHideableSupport;
import org.jblooming.operator.User;
import org.jblooming.security.Permission;
import org.jblooming.security.Securable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;

import java.util.Date;

/**
 * News (c) 2005 - Open Lab - www.open-lab.com
 */
public class News extends SecuredLoggableHideableSupport implements Securable {

  private String title;
  private String subTitle;
  private String text;
  private PersistentFile image;
  private Integer imageWidth = 0;
  private Integer imageHeight = 0;

  private Integer orderFactor;

  private Date startingDate;
  private Date endingDate;
  private boolean visible = true;

  public static final String NEWS = "NEWS";

  /**
   * CONSTRUCTORS
   */
  public News() {
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public String getSubTitle() {
    return subTitle;
  }

  public void setSubTitle(String subTitle) {
    this.subTitle = subTitle;
  }

  public Date getStartingDate() {
    return startingDate;
  }

  public void setStartingDate(Date startingDate) {
    this.startingDate = startingDate;
  }

  public Date getEndingDate() {
    return endingDate;
  }

  public void setEndingDate(Date endingDate) {
    this.endingDate = endingDate;
  }

  public String getText() {
    return text;
  }

  public void setText(String text) {
    this.text = text;
  }

  public PersistentFile getImage() {
    return image;
  }

  public void setImage(PersistentFile image) {
    this.image = image;
  }

  public boolean isVisible() {
    return visible;
  }

  public void setVisible(boolean visible) {
    this.visible = visible;
  }

  public boolean hasPermissionFor(User u, Permission p) {
    return getOwner() != null && getOwner().equals(u);
  }


  public Integer getImageWidth() {
    return imageWidth;
  }

  public void setImageWidth(Integer imageWidth) {
    this.imageWidth = imageWidth;
  }

  public Integer getImageHeight() {
    return imageHeight;
  }

  public void setImageHeight(Integer imageHeight) {
    this.imageHeight = imageHeight;
  }

  public Integer getOrderFactor() {
    return orderFactor;
  }

  public void setOrderFactor(Integer orderFactor) {
    this.orderFactor = orderFactor;
  }


}
