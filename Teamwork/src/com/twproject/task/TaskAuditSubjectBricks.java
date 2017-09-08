/**
 * 
 */
package com.twproject.task;

import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.html.display.Img;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.RestState;

import com.twproject.operator.TeamworkOperator;

/**
 * @author x-wang
 *
 */

public class TaskAuditSubjectBricks extends Bricks {

	public TaskAuditSubject mainObject;
	public TeamworkOperator logged;


	/**
	 * @param mainObject
	 */
	public TaskAuditSubjectBricks(TaskAuditSubject mainObject) {
		super();
		this.mainObject = mainObject;
	}

	public String getAvatarImageUrl() {
		String ret = "";
		if (this.mainObject.getPicture() != null) {
			PageSeed imgPs = this.mainObject.getPicture().getPageSeed(false);
			imgPs.disableCache = false;
			ret = imgPs.toLinkToHref();
		} else {
			ret = ApplicationState.contextPath + "/img/picture.png";
		}
		return ret;
	}


	public Img getAvatarImage(String size) {
		Img img = new Img(getAvatarImageUrl(), "");
		img.script = "class='face " + (JSP.ex(size) ? size : "") + "'";
		return img;
	}

	public static SmartCombo getSubjectCombo(String fieldName, RestState pageState)
			throws PersistenceException {

		String hql = "select subject.id, subject.description from "
				+ TaskAuditSubject.class.getName()
				+ " as subject ";

		String whereForId = "where subject.id = :" + SmartCombo.FILTER_PARAM_NAME;

		SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);

		QueryHelper queryHelperForFiltering = new QueryHelper(hql);



		String baseFilter = "";

		baseFilter = "(upper(subject.description || ' ' || subject.description) like :" + SmartCombo.FILTER_PARAM_NAME
				+ " or upper(subject.description || ' ' || subject.description) like:" + SmartCombo.FILTER_PARAM_NAME
				+ "  ) ";

		queryHelperForFiltering.addOQLClause(baseFilter);

		queryHelperForFiltering.addToHqlString(" order by subject.id");

		resources.queryHelperForFiltering = queryHelperForFiltering;
		resources.searchAll = true;
		resources.separator = "</td><td>";
		resources.fieldSize = 40;
		resources.convertToUpper = true;
		return resources;
	}
}
