package org.jblooming.flowork;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.designer.DesignerField;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;

import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class FieldAvailable extends IdentifiableSupport {

  private String kind;
  private String name;
  private String label;
  private String initialValue;
  /**
   * -1 means unbounded
   */
  private int cardinality = 1;
  private int columnLength = 20;


  public String getKind() {
    return kind;
  }

  public void setKind(String kind) {
    this.kind = kind;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getLabel() {
    return label;
  }

  public void setLabel(String label) {
    this.label = label;
  }

  public String getInitialValue() {
    return initialValue;
  }

  public void setInitialValue(String initialValue) {
    this.initialValue = initialValue;
  }

  public int getCardinality() {
    return cardinality;
  }

  public void setCardinality(int cardinality) {
    this.cardinality = cardinality;
  }

  public int getColumnLength() {
    return columnLength;
  }

  public void setColumnLength(int columnLength) {
    this.columnLength = columnLength;
  }

  public static DesignerField getDesignerField(boolean required, boolean readOnly, FieldAvailable fa, int index) {
    String indexNormalized = (index == 0 ? "" : index + "");
    DesignerField designerField = new DesignerField(fa.getKind(), fa.getName(), fa.getLabel(), required, readOnly, fa.getInitialValue());
    if (designerField.label == null || designerField.label.trim().length()==0)
      designerField.label = StringUtilities.deCamel(designerField.name);
    designerField.label+=(" "+indexNormalized);
    designerField.name+=indexNormalized;
    designerField.fieldSize = 0;
    designerField.maxLength = fa.getColumnLength();
    return designerField;
  }


  public static FieldAvailable load (Serializable id) throws FindByPrimaryKeyException {
    return (FieldAvailable) PersistenceHome.findByPrimaryKey(FieldAvailable.class,id);
  }
}
