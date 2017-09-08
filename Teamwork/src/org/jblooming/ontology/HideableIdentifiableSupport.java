package org.jblooming.ontology;

import javax.persistence.MappedSuperclass;
import javax.persistence.Column;
import java.util.Date;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 30-mar-2005 : 12.52.47
 */
@MappedSuperclass
public abstract class HideableIdentifiableSupport extends LoggableIdentifiableSupport implements Hideable {

  protected boolean hidden = false;
  private Date hiddenOn;
  private String hiddenBy;

  public HideableIdentifiableSupport() {
    super();
  }

  public HideableIdentifiableSupport(int id) {
    super(id);
  }

  @Column(nullable = true)
  public boolean isHidden() {
    return hidden;
  }

  public void setHidden(boolean hidden) {
    this.hidden = hidden;
  }

  public Date getHiddenOn() {
    return hiddenOn;
  }

  public void setHiddenOn(Date hiddenOn) {
    this.hiddenOn = hiddenOn;
  }

  public String getHiddenBy() {
    return hiddenBy;
  }

  public void setHiddenBy(String hiddenBy) {
    this.hiddenBy = hiddenBy;
  }

}
