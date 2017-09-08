package org.jblooming.logging;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.annotation.ElementType;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 12, 2007
 * Time: 11:39:05 AM
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Inaudited {
}
