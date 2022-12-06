package br.gov.lexml.parser.pl.ws.data.jaxb;

import org.pojomatic.Pojomatic;
import org.pojomatic.annotations.AutoProperty;

@AutoProperty
@jakarta.xml.bind.annotation.XmlTransient
public abstract class Base {
	  @Override final public boolean equals(Object o) {
	    return Pojomatic.equals(this, o);
	  }

	  @Override final public int hashCode() {
	    return Pojomatic.hashCode(this);
	  }

	  @Override final public String toString() {
	    return Pojomatic.toString(this);
	  }
}
