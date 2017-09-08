package org.jblooming.security;

import org.jblooming.PlatformRuntimeException;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.Set;

public class Permission implements Comparable, Serializable {

  public String name;

  public Permission() {
  }

  public Permission(String name) {
    this.name = name;
  }


  public String getName() {
    return name;
  }

  public int compareTo(Object o) {
    if (o == null)
      return 0;
    else
      return name.compareTo(((Permission) o).getName());
  }

  public boolean equals(Object o) {
    return this.compareTo(o) == 0;
  }

  /**
   * This method solves collection'identity
   */
  public int hashCode() {
    return getName().hashCode();
  }

  public static void addPermissions(Object permissionClassInstance, Set<Permission> permissions) {
    Field[] field = permissionClassInstance.getClass().getDeclaredFields();
    for (int i = 0; i < field.length; i++) {
      Field field1 = field[i];
      if (field1.getType().equals(Permission.class))
        try {
          Permission perm = (Permission) field1.get(permissionClassInstance);
          permissions.add(perm);
        } catch (IllegalAccessException e) {
          throw new PlatformRuntimeException(e);
        }
    }
  }

  public String toString() {
    return name;
  }
}

