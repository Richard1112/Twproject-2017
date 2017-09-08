package com.teamwork.expand;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.search.annotations.Analyzer;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.annotations.Fields;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.jblooming.logging.Auditable;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.VersionHome;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.remoteFile.Document;
import org.jblooming.security.SecurableWithArea;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

import com.opnlb.fulltext.Indexable;
import com.opnlb.fulltext.IndexingMachine;
import com.twproject.resource.Company;
import com.twproject.resource.Resource;
import com.twproject.task.Task;
@Indexed(index="fulltext")
public class TaskReport extends IndexableReport implements Indexable,Comparable, Auditable{
	public TaskReportBricks bricks = new TaskReportBricks(this);
	
	private  String dictionaryName;
	private PersistentFile persistentFile;
	private String reportTypeId;
	
	private String reportDepartId;
	
	private String content;
	
	private Date startDate;
	
	private Date endDate;
	private String version;
	private String originalFileName;
	
	private String year;
	private String taskId;
	
	private Task task;
	private Company company;
    private TaskReportType taskReportType;
	protected Date lastModified;
	protected String lastModifier;
	protected String creator;
	protected Date creationDate;
	
	  @DocumentId
	  @FieldBridge(impl = IntegerBridge.class)
	  public Serializable getId() {
	    return super.getId();
	  }

	  
	  
	public String getDictionaryName() {
		return dictionaryName;
	}

	public Company getCompany() {
		return company;
	}

	public void setCompany(Company company) {
		this.company = company;
	}

	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	public void setDictionaryName(String dictionaryName) {
		this.dictionaryName = dictionaryName;
	}

	
	public String version(){
		  if (version == null)
		      return VersionHome.VERSION_ROOT;
		    return VersionHome.nextVersion(version);
	}
	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	public String getLastModifier() {
		return lastModifier;
	}

	public void setLastModifier(String lastModifier) {
		this.lastModifier = lastModifier;
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public Date getCreationDate() {
		return creationDate;
	}

	public void setCreationDate(Date creationDate) {
		this.creationDate = creationDate;
	}

	public String getReportTypeId() {
		return reportTypeId;
	}

	public void setReportTypeId(String reportTypeId) {
		this.reportTypeId = reportTypeId;
	}

	public String getReportDepartId() {
		return reportDepartId;
	}

	public void setReportDepartId(String reportDepartId) {
		this.reportDepartId = reportDepartId;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getEndDate() {
		return endDate;
	}

	public void setEndDate(Date endDate) {
		this.endDate = endDate;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}
	
	public static TaskReport load(Serializable mainObjectId) throws FindByPrimaryKeyException {
	    return load(mainObjectId, PersistenceContext.getDefaultPersistenceContext());
}


  public static TaskReport load(Serializable mainObjectId, PersistenceContext pc) throws FindByPrimaryKeyException {
    return (TaskReport) PersistenceHome.findByPrimaryKey(TaskReport.class, mainObjectId, pc);
  }
  
  
  
//findUniqueObject(Class clazz, String field, Object value, PersistenceContext pc)
  public static TaskReport loadInfoByFileName(Object value) throws PersistenceException {
    return (TaskReport) PersistenceHome.findUniqueObject(TaskReport.class,"originalFileName", value, PersistenceContext.getDefaultPersistenceContext());
  }


public Task getTask() {
	return task;
}

public void setTask(Task task) {
	this.task = task;
}

public TaskReportType getTaskReportType() {
	return taskReportType;
}
public PersistentFile getFile() {
    return persistentFile;
  }

  public void setFile(PersistentFile file) {
    this.persistentFile = file;
  }

public PersistentFile getPersistentFile() {
	return persistentFile;
}

public void setPersistentFile(PersistentFile persistentFile) {
	this.persistentFile = persistentFile;
}

public String getVersion() {
	return version;
}

public void setVersion(String version) {
	this.version = version;
}

public String getOriginalFileName() {
	return originalFileName;
}

public void setOriginalFileName(String originalFileName) {
	this.originalFileName = originalFileName;
}

public void setTaskReportType(TaskReportType taskReportType) {
	this.taskReportType = taskReportType;
}

public Identifiable getReferral() {

    Identifiable result = null;

    if (!isNew()) {
      if (getTask() != null)
        result = getTask();
    }
    return result;
  }

@Fields({
    @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
    @Field(name = "content")
  })
  public String getContentForIndexing() {

    if (getFile() != null && getReferral() != null && ((SecurableWithArea) getReferral()).getArea() != null) {
      IndexingMachine.addToBeIndexed(this, ((SecurableWithArea) getReferral()).getArea().getId(), getFile());
    }
    return getAbstractForIndexing();

  }
public String getAbstractForIndexing() {

    return JSP.w(
      JSP.w(getCreator()) + " " +
        JSP.w(getClass()) + " " +
        JSP.w(getOriginalFileName()) + " " +
        JSP.w(getName()) + " " +
        JSP.w(getContent()) + " " 
    );
  }

public boolean existsFile() {
Boolean exists=false;
      if (getFile() != null) {

        try {
          getFile().getInputStream(PersistenceContext.get(Document.class));
          exists = true;
        } catch (Throwable e) {
          Tracer.platformLogger.debug("File does not exist: "+getFile().serialize(),e);
        }
      }

  
    return exists;
  }



}
