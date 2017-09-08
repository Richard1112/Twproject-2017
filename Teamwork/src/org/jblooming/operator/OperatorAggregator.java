package org.jblooming.operator;

import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.QueryException;

import java.util.Collection;


/**
 * This interface must be implemented by all objects that can be used to group operators e.g.:
 * Task -> all the operator assigned to it, and all the operator assigned to its
 * parent if inherit, and all the users assigned to its children if propagate
 * Department -> all the operator associated to it, and all the users assigned to its
 * parent if inherit, and all the users assigned to its children if propagate
 * Skill -> all the resources associated to it
 * Operator -> return (Operator) itself
 *
 * @author Pietro Polsinelli
 * @author Roberto Bicchierai
 * @version 2 alpha
 * @since JDK 1.4.1
 */
public interface OperatorAggregator {

  public Collection<Operator> getOperators() ;

  public boolean isOperatorIn(Operator o) throws FindException, QueryException;
}

