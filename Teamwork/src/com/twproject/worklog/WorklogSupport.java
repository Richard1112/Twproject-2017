package com.twproject.worklog;

import com.opnlb.fulltext.Indexable;
import com.twproject.task.Assignment;
import com.twproject.task.Issue;
import com.twproject.worklog.businessLogic.WorklogBricks;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.*;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.designer.DesignerField;
import org.jblooming.ontology.SecuredLoggableHideableSupport;
import org.jblooming.security.Area;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 18, 2008
 * Time: 9:35:35 AM
 */
@Entity
@Inheritance(strategy=InheritanceType.SINGLE_TABLE)
@Table(name = "twk_worklog")
@DiscriminatorColumn(
name="discriminator",
discriminatorType=DiscriminatorType.STRING
)
public abstract class WorklogSupport extends SecuredLoggableHideableSupport implements Indexable {


  private long duration;

  private String action;
  private Date inserted;
  protected Assignment assig;
  private Issue issue;
  private WorklogStatus status;
  private String source;


  private String customField1;
  private String customField2;
  private String customField3;
  private String customField4;


  @Id
  @Type(type = "string")
  @DocumentId
  @FieldBridge(impl = StringBridge.class)
  public Serializable getId() {
    return super.getId();
  }

  @IndexedEmbedded
  @Transient
  public Area getArea() {
    return getAssig().getTask().getArea();
  }

  @Column(length = 2000)
  public String getAction() {
    return action;
  }

  public void setAction(String action) {
    this.action = action;
  }

  public long getDuration() {
    return duration;
  }

  public void setDuration(long duration) {
    this.duration = duration;
  }

  public void setInserted(Date inserted) {
    this.inserted = inserted;
  }

  @org.hibernate.annotations.Index(name = "idx_worklog_inserted")
  public Date getInserted() {
    return inserted;
  }

  @ManyToOne(targetEntity = Assignment.class)
  @ForeignKey(name = "fk_worklog_assig")
  @org.hibernate.annotations.Index(name = "idx_worklog_assig")
  public Assignment getAssig() {
    return assig;
  }

  public void setAssig(Assignment assig) {
    this.assig = assig;
  }

  @Transient
  public String getName() {
    return getId() + "";
  }

  @ManyToOne(targetEntity = Issue.class)
  @ForeignKey(name = "fk_worklog_issue")
  @org.hibernate.annotations.Index(name = "idx_worklog_issue")
  public Issue getIssue() {
    return issue;
  }

  public void setIssue(Issue issue) {
    this.issue = issue;
  }

  @Transient
  public String getDisplayName(PageState pageState) {
    return DateUtilities.dateToString(inserted)+":"+
      assig.getDisplayName()+ " " + JSP.limWr(JSP.w(action),10)+" - dur."+
      DateUtilities.getMillisInHoursMinutes(duration);
  }

  @Transient
  public String getAbstractForIndexing() {
    return JSP.w(getAction())+
    JSP.w(getCustomField1()) + " " +
    JSP.w(getCustomField2()) + " " +
    JSP.w(getCustomField3()) + " " +
    JSP.w(getCustomField4());
  }

  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
          })
    @Transient
  private  String getContentForIndexing() {
    return getAbstractForIndexing();
  }


  @ManyToOne(targetEntity = WorklogStatus.class)
  @ForeignKey(name = "fk_worklog_wklsts")
  @org.hibernate.annotations.Index(name = "idx_worklog_wklsts")
  @JoinColumn (name = "statusx")
  public WorklogStatus getStatus() {
    return status;
  }

  public void setStatus(WorklogStatus status) {
    this.status = status;
  }

  public String getSource() {
    return source;
  }

  public void setSource(String source) {
    this.source = source;
  }

  public String getCustomField1() {
    return customField1;
  }

  public void setCustomField1(String customField1) {
    this.customField1 = customField1;
  }

  public String getCustomField2() {
    return customField2;
  }

  public void setCustomField2(String customField2) {
    this.customField2 = customField2;
  }

  public String getCustomField3() {
    return customField3;
  }

  public void setCustomField3(String customField3) {
    this.customField3 = customField3;
  }

  public String getCustomField4() {
    return customField4;
  }

  public void setCustomField4(String customField4) {
    this.customField4 = customField4;
  }


  @Transient
  public static boolean hasCustomField() {
    return DesignerField.hasCustomField("WORKLOG_CUSTOM_FIELD_", 4);
  }

}
