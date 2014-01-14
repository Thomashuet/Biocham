package fr.inria.contraintes.biocham.plotting;


public class PlotProperties {

	
	 String plotTitle;
	 String xLabel;
	 String yLabel;
	 String xTicks;
	 String yTicks;
	 String marks;
	 Boolean autorange;
	 String xRange;
	 String yRange;
	 Boolean grid;
	 Boolean impulses;
	 Boolean lines;
	 Boolean bars;
	 Boolean xLogAxis;
	 Boolean yLogAxis;
	 
	 
	public Boolean getAutorange() {
		return autorange;
	}
	public void setAutorange(Boolean autorange) {
		this.autorange = autorange;
	}
	public Boolean getBars() {
		return bars;
	}
	public void setBars(Boolean bars) {
		this.bars = bars;
	}
	public Boolean getGrid() {
		return grid;
	}
	public void setGrid(Boolean grid) {
		this.grid = grid;
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
	public String getPlotTitle() {
		return plotTitle;
	}
	public void setPlotTitle(String plotTitle) {
		this.plotTitle = plotTitle;
	}
	public String getXLabel() {
		return xLabel;
	}
	public void setXLabel(String label) {
		xLabel = label;
	}
	public Boolean getXLogAxis() {
		return xLogAxis;
	}
	public void setXLogAxis(Boolean logAxis) {
		xLogAxis = logAxis;
	}
	public String getXRange() {
		return xRange;
	}
	public void setXRange(String range) {
		xRange = range;
	}
	public String getXTicks() {
		return xTicks;
	}
	public void setXTicks(String ticks) {
		xTicks = ticks;
	}
	public String getYLabel() {
		return yLabel;
	}
	public void setYLabel(String label) {
		yLabel = label;
	}
	public Boolean getYLogAxis() {
		return yLogAxis;
	}
	public void setYLogAxis(Boolean logAxis) {
		yLogAxis = logAxis;
	}
	public String getYRange() {
		return yRange;
	}
	public void setYRange(String range) {
		yRange = range;
	}
	public String getYTicks() {
		return yTicks;
	}
	public void setYTicks(String ticks) {
		yTicks = ticks;
	}
}
