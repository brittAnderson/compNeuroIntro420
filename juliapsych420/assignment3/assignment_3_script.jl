include("DampedOscillatorClass.jl")

using .DampedOscillatorClass

initialPosition = 10.0 # meters
initialVelocity = 0.0 # meters
springConstant = 3.0 # Newtons / Meter
deltaT = 0.1 # second 
runTime = 25. # seconds

dampeningConstants = 0.1:0.1:1.0 # Include 1.0

for dampeningConstant in dampeningConstants
    osc = initializeOscillator(
        initialPosition,
        initialVelocity,
        springConstant,
        dampeningConstant,
        deltaT
    )

    releaseSpring(osc, runTime)

    plotSpring(osc, "position")
end