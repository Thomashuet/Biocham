package fr.inria.contraintes.biocham.plotting;

import java.awt.Color;
import java.util.ArrayList;
import java.util.StringTokenizer;



public class DatasetProperties {

	
		String datasetName;
		String values;
		String marks;
		Boolean impulses;
		Boolean bars;
		Boolean lines;
		Color color;
		
		
		
		public Boolean getBars() {
			return bars;
		}
		public void setBars(Boolean bars) {
			this.bars = bars;
		}
		public Color getColor() {
			return color;
		}
		public void setColor(Color color) {
			this.color = color;
		}
		public String getDatasetName() {
			return datasetName;
		}
		public void setDatasetName(String datasetName) {
			this.datasetName = datasetName;
		}
		public Boolean getImpulses() {
			return impulses;
		}
		public void setImpulses(Boolean impulses) {
			this.impulses = impulses;
		}
		public Boolean getLines() {
			return lines;
		}
		public void setLines(Boolean lines) {
			this.lines = lines;
		}
		public String getMarks() {
			return marks;
		}
		public void setMarks(String marks) {
			this.marks = marks;
		}
		public ArrayList<String> getValues() {
			//1,2,3;4,5,6;7,8,9; //or, 1,2;3,4;
			ArrayList<String> vals=new ArrayList<String>();
			StringTokenizer st=new StringTokenizer(values,";");
			while(st.hasMoreTokens()){
				String tv=st.nextToken();
				vals.add(tv);
			}
			return vals;
		}
		public void setValues(String values) {
			this.values = values;
		}
		
		
		
}
