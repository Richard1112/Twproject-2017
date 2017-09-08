/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;

import net.sf.json.JSONObject;


/**
 * 会议室竖表
 * 
 * @author WangXi
 * @create July 3, 2017
 */
@Entity
@Table(name = "twk_task_customer_field")
public class TaskCustomerField extends LoggableIdentifiableSupport {
	private String description;
	private DataType dataType;
	private String name;
	private String taskType;
	// private int isDefault;

	private List<TaskCustomerFieldRelation> relations = new java.util.LinkedList<>();


	public TaskCustomerField() {
	}

	@Override
	@Id
	@Type(type = "int")
	@FieldBridge(impl = IntegerBridge.class)
	public Serializable getId() {
		return super.getId();
	}

	public void setId(int id) {
		this.id = id;
	}

	@OneToMany(cascade = { javax.persistence.CascadeType.REMOVE }, targetEntity = TaskCustomerFieldRelation.class)
	@JoinColumn(name = "field")
	public List<TaskCustomerFieldRelation> getRelations() {
		return relations;
	}

	public void setRelations(List<TaskCustomerFieldRelation> relations) {
		this.relations = relations;
	}

	public static TaskCustomerField load(Serializable id) throws FindByPrimaryKeyException {
		return (TaskCustomerField) PersistenceHome.findByPrimaryKey(TaskCustomerField.class, id);
	}

	public static TaskCustomerField load(String id) throws FindByPrimaryKeyException {
		return (TaskCustomerField) PersistenceHome.findByPrimaryKey(TaskCustomerField.class, id);
	}

	public static TaskCustomerField loadByName(String code) throws PersistenceException {
		return (TaskCustomerField) PersistenceHome.findUnique(TaskCustomerField.class, "name", code);
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@ManyToOne(targetEntity = DataType.class)
	@ForeignKey(name = "fk_field_type")
	@Index(name = "idx_field_type")
	public DataType getDataType() {
		return dataType;
	}

	public void setDataType(DataType dataType) {
		this.dataType = dataType;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTaskType() {
		return taskType;
	}

	public void setTaskType(String taskType) {
		this.taskType = taskType;
	}

	// public int getIsDefault() {
	// return isDefault;
	// }
	//
	// public void setIsDefault(int isDefault) {
	// this.isDefault = isDefault;
	// }


	@Override
	public JSONObject jsonify() {
		JSONObject jso = super.jsonify();

		jso.element("id", getId());
		jso.element("description", JSP.encode(getDescription()));
		jso.element("dataTypeId", getDataType().getIntId());
		jso.element("dataType", JSP.encode(getDataType().getName()));
		jso.element("name", JSP.encode(getName()));
		// jso.element("isDefault", getIsDefault());

		return jso;
	}

}
