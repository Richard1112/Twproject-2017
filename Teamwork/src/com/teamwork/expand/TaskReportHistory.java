package com.teamwork.expand;

import java.io.Serializable;
import java.util.Date;

import org.jblooming.logging.Auditable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.hibernate.PersistenceContext;

import com.twproject.resource.Company;
import com.twproject.task.Task;

public class TaskReportHistory extends IndexableReport implements Comparable, Auditable{
	private  String dictionaryName;
	private PersistentFile persistentFile;
	private String reportTypeId;
	
	private String reportDepartId;
	
	private String content;
	private String parentId;
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
	public String getDictionaryName() {
		return dictionaryName;
	}
	public void setDictionaryName(String dictionaryName) {
		this.dictionaryName = dictionaryName;
	}
	
	public String getParentId() {
		return parentId;
	}
	public void setParentId(String parentId) {
		this.parentId = parentId;
	}
	public PersistentFile getPersistentFile() {
		return persistentFile;
	}
	public void setPersistentFile(PersistentFile persistentFile) {
		this.persistentFile = persistentFile;
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
	public String getYear() {
		return year;
	}
	public void setYear(String year) {
		this.year = year;
	}
	public String getTaskId() {
		return taskId;
	}
	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}
	public Task getTask() {
		return task;
	}
	public void setTask(Task task) {
		this.task = task;
	}
	public Company getCompany() {
		return company;
	}
	public void setCompany(Company company) {
		this.company = company;
	}
	public TaskReportType getTaskReportType() {
		return taskReportType;
	}
	public void setTaskReportType(TaskReportType taskReportType) {
		this.taskReportType = taskReportType;
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
	  public void setFile(PersistentFile file) {
		    this.persistentFile = file;
		  }

	public static TaskReportHistory load(Serializable mainObjectId) throws FindByPrimaryKeyException {
	    return load(mainObjectId, PersistenceContext.getDefaultPersistenceContext());
}


  public static TaskReportHistory load(Serializable mainObjectId, PersistenceContext pc) throws FindByPrimaryKeyException {
    return (TaskReportHistory) PersistenceHome.findByPrimaryKey(TaskReportHistory.class, mainObjectId, pc);
  }
	
}
