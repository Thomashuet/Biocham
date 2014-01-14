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
 * JavaPlot.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import javax.swing.*;
import javax.swing.table.*;
import java.awt.*;
import java.io.*;
import java.util.*;

public class DataTable extends JScrollPane {
   JTable table;
   Float[][] data;    // data and time can be accessed by repaint()!
   String[] name;

   DataTable(JavaPlot j) {
      super();
      data = j.fulldata;
      name = j.fullname;

      TableModel dataModel = new AbstractTableModel () {
         public String getColumnName(int col) {
            return name[col];
         }
         public int getRowCount() {
            return data[0].length;
         }
         public int getColumnCount() {
            return data.length;
         }
         public Object getValueAt(int row, int col) {
            return data[col][data[0].length-row-1];
         }
         public boolean isCellEditable(int row, int col) {
            return false;
         }
         public void setValueAt(Object value, int row, int col) {}
      };
      table = new JTable(dataModel);
      //table = new JTable(data, name);
      table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
      setFocusable(false);
      setViewportView(table);
      validate();
      table.setRowSelectionAllowed(false);
      table.setColumnSelectionAllowed(true);
      table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
   }
}

