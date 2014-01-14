/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2008, INRIA, Projet Contraintes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * FileFilterBiocham.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;


/**
 * A class that keeps files different from Biocham files (with .bc extension)
 * from appearing in the directory listing.
 *            
 * @author Sylvain Soliman         
*/
public class FileFilterBiocham extends FileFilter {

   public boolean accept(File f) {
      return (f.isDirectory() || f.getName().endsWith(".bc") || f.getName().endsWith(".xml"));
   }

   public String getDescription() {
      return "Biocham files only";
   }
}

