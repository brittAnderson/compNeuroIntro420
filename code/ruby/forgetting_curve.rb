#!/usr/bin/env ruby
# Ebbinghaus Forgetting Curve
#
# Models memory retention over time using the classic exponential decay
# formula popularized by Hermann Ebbinghaus:
#
#     R(t) = e^(-t / S)
#
# where:
#   R = retention (proportion of information remembered, 0..1)
#   t = time elapsed since learning (in the same units as S)
#   S = "strength of memory" — a decay constant. Larger S = slower forgetting.
#
# This script computes the curve and renders it two ways:
#   1. An ASCII plot printed straight to the terminal (no dependencies)
#   2. A standalone SVG file you can open in any browser
#
# Usage:
#   ruby ebbinghaus_forgetting_curve.rb
#   ruby ebbinghaus_forgetting_curve.rb --strength 1.5 --hours 168
#
# Optional: pass multiple strengths to compare curves, e.g. --strength 0.5,1,2

require 'optparse'

# ---------------------------------------------------------------------------
# 1. Parse options
# ---------------------------------------------------------------------------
options = {
  strengths: [24.0],  # memory strength constant(s), S (in hours; 24h ~ typical single-exposure decay)
  max_time:  168.0,   # how far out to plot, in hours (default: 1 week)
  points:    200,      # resolution of the curve
  out_file:  'ebbinghaus_curve.svg'
}

OptionParser.new do |opts|
  opts.banner = 'Usage: ruby ebbinghaus_forgetting_curve.rb [options]'

  opts.on('--strength S1,S2,...', Array, 'Memory strength constant(s), comma-separated (default: 1.0)') do |vals|
    options[:strengths] = vals.map(&:to_f)
  end

  opts.on('--hours HOURS', Float, 'How many hours to plot out to (default: 168 = 1 week)') do |v|
    options[:max_time] = v
  end

  opts.on('--out FILE', String, 'Output SVG filename (default: ebbinghaus_curve.svg)') do |v|
    options[:out_file] = v
  end
end.parse!

# ---------------------------------------------------------------------------
# 2. The forgetting curve itself
# ---------------------------------------------------------------------------
def retention(t, strength)
  Math.exp(-t.to_f / strength)
end

def build_curve(strength, max_time, points)
  step = max_time / (points - 1)
  (0...points).map do |i|
    t = i * step
    [t, retention(t, strength)]
  end
end

curves = options[:strengths].map { |s| [s, build_curve(s, options[:max_time], options[:points])] }

# ---------------------------------------------------------------------------
# 3. SVG plot (open in a browser for a proper chart)
# ---------------------------------------------------------------------------
def build_svg(curves, max_time, out_file)
  w, h = 760, 460
  margin = 60
  plot_w = w - 2 * margin
  plot_h = h - 2 * margin
  colors = %w[#2563eb #dc2626 #16a34a #d97706 #7c3aed]

  x_of = ->(t) { margin + (t / max_time) * plot_w }
  y_of = ->(r) { margin + (1 - r) * plot_h }

  svg = +"<svg xmlns='http://www.w3.org/2000/svg' width='#{w}' height='#{h}' viewBox='0 0 #{w} #{h}' font-family='Helvetica, Arial, sans-serif'>\n"
  svg << "<rect width='#{w}' height='#{h}' fill='white'/>\n"
  svg << "<text x='#{w / 2}' y='30' font-size='18' text-anchor='middle' font-weight='bold'>Ebbinghaus Forgetting Curve</text>\n"

  # Axes
  svg << "<line x1='#{margin}' y1='#{margin}' x2='#{margin}' y2='#{h - margin}' stroke='black' stroke-width='1.5'/>\n"
  svg << "<line x1='#{margin}' y1='#{h - margin}' x2='#{w - margin}' y2='#{h - margin}' stroke='black' stroke-width='1.5'/>\n"

  # Y gridlines/labels (retention 0.0 - 1.0)
  0.step(10, 2) do |i|
    frac = i / 10.0
    y = y_of.call(frac)
    svg << "<line x1='#{margin}' y1='#{y}' x2='#{w - margin}' y2='#{y}' stroke='#e5e7eb' stroke-width='1'/>\n"
    svg << "<text x='#{margin - 10}' y='#{y + 4}' font-size='11' text-anchor='end'>#{format('%.1f', frac)}</text>\n"
  end

  # X labels (time)
  5.times do |i|
    t = max_time * i / 4.0
    x = x_of.call(t)
    svg << "<text x='#{x}' y='#{h - margin + 20}' font-size='11' text-anchor='middle'>#{t.round(1)}h</text>\n"
  end

  svg << "<text x='#{w / 2}' y='#{h - 12}' font-size='12' text-anchor='middle'>Time since learning</text>\n"
  svg << "<text x='16' y='#{h / 2}' font-size='12' text-anchor='middle' transform='rotate(-90 16 #{h / 2})'>Retention</text>\n"

  # Curves
  curves.each_with_index do |(strength, data), i|
    color = colors[i % colors.length]
    path = data.map.with_index { |(t, r), idx| "#{idx == 0 ? 'M' : 'L'}#{x_of.call(t).round(2)},#{y_of.call(r).round(2)}" }.join(' ')
    svg << "<path d='#{path}' fill='none' stroke='#{color}' stroke-width='2.5'/>\n"
  end

  # Legend
  legend_y = margin + 10
  curves.each_with_index do |(strength, _), i|
    color = colors[i % colors.length]
    ly = legend_y + i * 18
    svg << "<line x1='#{w - margin - 90}' y1='#{ly}' x2='#{w - margin - 65}' y2='#{ly}' stroke='#{color}' stroke-width='3'/>\n"
    svg << "<text x='#{w - margin - 58}' y='#{ly + 4}' font-size='11'>S = #{strength}</text>\n"
  end

  svg << "</svg>\n"

  File.write(out_file, svg)
  puts "SVG plot written to: #{out_file}"
end

build_svg(curves, options[:max_time], options[:out_file])