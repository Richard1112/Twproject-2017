package org.jblooming.designer;

import javax.servlet.jsp.PageContext;
import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class DesignerLoggable extends DesignerField {

  public enum LogType {LAST_MODIFIED, LAST_MODIFIER, CREATOR, CREATION_DATE }

  public LogType type;

  public DesignerLoggable(LogType type, String fieldName, String label,String initialValue) {
    if (DesignerLoggable.LogType.LAST_MODIFIED.equals(type) || DesignerLoggable.LogType.CREATION_DATE.equals(type))
      this.kind = Date.class.getName();
    else if (DesignerLoggable.LogType.LAST_MODIFIER.equals(type) || DesignerLoggable.LogType.CREATOR.equals(type))
      this.kind = String.class.getName();
    this.name = fieldName;
    this.label = label;
    this.initialValue = initialValue;
    this.type = type;
    this.readOnly = true;
    this.required = false;
  }

  public void toHtml(PageContext pageContext) {
    if (DesignerLoggable.LogType.LAST_MODIFIED.equals(type) || DesignerLoggable.LogType.CREATION_DATE.equals(type))
      this.kind = Date.class.getName();
    else if (DesignerLoggable.LogType.LAST_MODIFIER.equals(type) || DesignerLoggable.LogType.CREATOR.equals(type))
      this.kind = String.class.getName();
    this.readOnly = true;
    this.required = false;
    super.toHtml(pageContext);
  }


}
