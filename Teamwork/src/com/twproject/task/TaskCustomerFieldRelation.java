/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.oql.OqlQuery;
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
@Table(name = "twk_task_cus_relation")
public class TaskCustomerFieldRelation extends LoggableIdentifiableSupport {

	private String value;

	private Task task;

	private TaskCustomerField field;

	public TaskCustomerFieldRelation() {
	}

	/**
	 * @param task
	 * @param field
	 */
	public TaskCustomerFieldRelation(Task task, TaskCustomerField field, String value) {
		this.task = task;
		this.field = field;
		this.value = value;
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

	public static TaskCustomerFieldRelation load(Serializable id) throws FindByPrimaryKeyException {
		return (TaskCustomerFieldRelation) PersistenceHome.findByPrimaryKey(TaskCustomerFieldRelation.class, id);
	}

	public static void deleteAll(Serializable taskId, List<Integer> rlids) throws PersistenceException {
		String hql = "from " + TaskCustomerFieldRelation.class.getName() + " as obj where obj.task.id = :aparam";
		
		try {
			if (rlids!=null) {
				hql += " and obj.id not in (:alist)";
			}
			final OqlQuery oqlQuery = new OqlQuery(hql);
			Query query = oqlQuery.getQuery();
			query.setParameter("aparam", taskId);
			if (rlids!=null) {
				query.setParameterList("alist", rlids);
			}
			List<TaskCustomerFieldRelation> list = query.list();
			if (JSP.ex(list)) {
				for (TaskCustomerFieldRelation rl : list) {
					rl.remove();
				}
			}
		} catch (HibernateException e) {
			throw new PersistenceException(e);
		}
	}

	@ManyToOne(targetEntity = Task.class)
	@ForeignKey(name = "fk_field_relation")
	@Index(name = "idx_field_relation")
	public Task getTask() {
		return task;
	}

	public void setTask(Task task) {
		this.task = task;
	}

	@ManyToOne(targetEntity = TaskCustomerField.class)
	@ForeignKey(name = "fk_task_field")
	@Index(name = "idx_task_field")
	public TaskCustomerField getField() {
		return field;
	}


	public void setField(TaskCustomerField field) {
		this.field = field;
	}


	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}


	@Override
	public JSONObject jsonify() {
		JSONObject jso = super.jsonify();

		jso.element("id", getId());
		jso.element("value", JSP.encode(getValue()));
		jso.element("name", JSP.encode(field.getName()));
		jso.element("type", JSP.encode(field.getDataType().getName()));
		jso.element("description", JSP.encode(field.getDescription()));
		jso.element("fieldId", field.getId());

		return jso;
	}

}
