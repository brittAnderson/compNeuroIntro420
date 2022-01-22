module DampedOscillatorClass

using Plots

export DampedOscillator, releaseSpring, initializeOscillator, plotSpring

struct DampedOscillator
    springConstant::Float64
    dampeningConstant::Float64
    deltaT::Float64
	positions::Vector{Float64}
    velocities::Vector{Float64}
    accelerations::Vector{Float64}
    times::Vector{Float64}
end


"""
_updateAcceleration(osc)
Appends a new entry into osc.accelerations for the current time step.
This function shoud not be directly called. Instead use releaseSpring.
"""
function _updateAcceleration(osc:: DampedOscillator)
    acc = -1 * osc.positions[end] * osc.springConstant - osc.dampeningConstant * osc.velocities[end]
    push!(osc.accelerations, acc)
end


"""
_updateVelocity
Appends a new entry into osc.velocities for the current time step.
This function shoud not be directly called. Instead use releaseSpring.
"""
function _updateVelocity(osc:: DampedOscillator)
    vel = osc.velocities[end] + osc.accelerations[end]*osc.deltaT
    push!(osc.velocities, vel)
end


"""
_updatePosition
Appends a new entry into osc.positions for the current time step.
This function shoud not be directly called. Instead use releaseSpring.
"""
function _updatePosition(osc:: DampedOscillator)
    pos = osc.positions[end] + osc.velocities[end]*osc.deltaT
    push!(osc.positions, pos)
end


"""
initializeOscillator(initialPosition,initialVelocity,springConstant,dampeningConstant)

Initializes a DampedOscillator structure with the appropriate values and types given the above inputs.
"""
function initializeOscillator(
	initialPosition::Float64,
	initialVelocity::Float64,
	springConstant::Float64,
	dampeningConstant::Float64,
	deltaT::Float64,
)
	positions=[initialPosition]
	velocities=[initialVelocity]
	times=[0.]
	accelerations=[]

	osc = DampedOscillator(
		springConstant,
		dampeningConstant,
		deltaT,
		positions,
		velocities,
		accelerations,
		times
	)

	_updateAcceleration(osc)
	return osc
end


"""
releaseSpring(osc, stopTime)

Calcualte the acceleration, velocity, and position of the oscillator at each time step until stopTime is reached
"""
function releaseSpring(osc::DampedOscillator, stopTime::Float64)
    if (stopTime<osc.deltaT)
        ErrorException("Time must be larger than $(osc.deltaT)")
    end

    currentTime = osc.times[end]
    while (currentTime <= stopTime + osc.deltaT)
        currentTime += osc.deltaT
        push!(osc.times, currentTime)
        _updateAcceleration(osc)
        _updateVelocity(osc)
        _updatePosition(osc)
    end
end


"""
plotSpring(osc, which)

Plots a time series of either the position, velocity, or acceleration for the input oscillator.
"""
function plotSpring(osc::DampedOscillator, which::String = "position")
	if which=="position"
		y = osc.positions
		yLabel = "Position (m)"
	elseif which == "velocity"
		y = osc.velocities
		yLabel = "Velocity (m/s)"
	else
		y = osc.accelerations
		yLabel = "Acceleration (m/s^2)"
	end

	plot(osc.times, y, lab="Dampening Factor: $(osc.dampeningConstant)")
	plot!(
		title = "$(yLabel) versus Time (s) for a Dampening Oscillator",
		xlabel = "Time (s)",
		ylabel = yLabel
	)
	dampeningConstantName = replace(string(osc.dampeningConstant), "."=>"_")
	savefig("./assignment3/julia_plots/$(yLabel)_with_dampening_$(dampeningConstantName).png")
end

end # MODULE