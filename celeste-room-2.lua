-- ~celeste~
-- matt thorson + noel berry

-- globals --
-------------

room = { x=0, y=0 }
objects = {}
types = {}
freeze=0
will_restart=false
delay_restart=0
pause_player=false

k_left=0
k_right=1
k_up=2
k_down=3
k_jump=4
k_dash=5

-- entry point --
-----------------

function _init()
	deaths=0
	max_djump=1

	load_room(1, 0)
end

function level_index()
	return room.x%8+room.y*8
end

-- player entity --
-------------------

player = 
{
	init=function(this) 
		this.p_jump=false
		this.p_dash=false
		this.grace=0
		this.djump=max_djump
		this.dash_time=0
		this.dash_effect_time=0
		this.dash_target={x=0,y=0}
		this.dash_accel={x=0,y=0}
		this.hitbox = {x=1,y=3,w=6,h=5}
	end,
	update=function(this)
		if (pause_player) then
			return
		end
		
		local input = btn(k_right) and 1 or (btn(k_left) and -1 or 0)
		
		-- spikes collide
		if spikes_at(this.x+this.hitbox.x,this.y+this.hitbox.y,this.hitbox.w,this.hitbox.h,this.spd.x,this.spd.y) then
		 kill_player(this) end
		 
		-- bottom death
		if this.y>128 then
			kill_player(this) end

		local on_ground=this.is_solid(0,1)
		
		local jump = btn(k_jump) and not this.p_jump
		this.p_jump = btn(k_jump)
		
		local dash = btn(k_dash) and not this.p_dash
		this.p_dash = btn(k_dash)
		
		if on_ground then
			this.grace=6
			if this.djump<max_djump then
			 this.djump=max_djump
			end
		elseif this.grace > 0 then
		 this.grace = this.grace - (1)
		end

		this.dash_effect_time = this.dash_effect_time - (1)
  if this.dash_time > 0 then
  	this.dash_time = this.dash_time - (1)
  	this.spd.x=appr(this.spd.x,this.dash_target.x,this.dash_accel.x)
  	this.spd.y=appr(this.spd.y,this.dash_target.y,this.dash_accel.y)  
  else

			-- move
			local maxrun=1
			local accel=0.6
			local deccel=0.15
			
			if not on_ground then
				accel=0.4
			end
		
			if abs(this.spd.x) > maxrun then
		 	this.spd.x=appr(this.spd.x,sign(this.spd.x)*maxrun,deccel)
			else
				this.spd.x=appr(this.spd.x,input*maxrun,accel)
			end
			
			--facing
			if this.spd.x~=0 then
				this.flip.x=(this.spd.x<0)
			end

			-- gravity
			local maxfall=2
			local gravity=0.21

  	if abs(this.spd.y) <= 0.15 then
   	gravity = gravity * (0.5)
			end
		
			-- wall slide
			if input~=0 and this.is_solid(input,0) then
		 	maxfall=0.4
			end

			if not on_ground then
				this.spd.y=appr(this.spd.y,maxfall,gravity)
			end

			-- jump
			if jump then
		 	if this.grace>0 then
		  	-- normal jump
		  	this.grace=0
					this.spd.y=-2
				else
					-- wall jump
					local wall_dir=(this.is_solid(-3,0) and -1 or this.is_solid(3,0) and 1 or 0)
					if wall_dir~=0 then
			 		this.spd.y=-2
			 		this.spd.x=-wall_dir*(maxrun+1)
					end
				end
			end
		
			-- dash
			local d_full=5
			local d_half=d_full*0.70710678118
		
			if this.djump>0 and dash then
		 	this.djump = this.djump - (1		)
		 	this.dash_time=4
		 	this.dash_effect_time=10
		 	local v_input=(btn(k_up) and -1 or (btn(k_down) and 1 or 0))
		 	if input~=0 then
		  	if v_input~=0 then
		   	this.spd.x=input*d_half
		   	this.spd.y=v_input*d_half
		  	else
		   	this.spd.x=input*d_full
		   	this.spd.y=0
		  	end
		 	elseif v_input~=0 then
		 		this.spd.x=0
		 		this.spd.y=v_input*d_full
		 	else
		 		this.spd.x=(this.flip.x and -1 or 1)
		  	this.spd.y=0
		 	end

		 	freeze=2
		 	this.dash_target.x=2*sign(this.spd.x)
		 	this.dash_target.y=2*sign(this.spd.y)
		 	this.dash_accel.x=1.5
		 	this.dash_accel.y=1.5
		 	
		 	if this.spd.y<0 then
		 	 this.dash_target.y = this.dash_target.y * (.75)
		 	end
		 	
		 	if this.spd.y~=0 then
		 	 this.dash_accel.x = this.dash_accel.x * (0.70710678118)
		 	end
		 	if this.spd.x~=0 then
		 	 this.dash_accel.y = this.dash_accel.y * (0.70710678118)
		 	end	 	 
			end
		
		end
		
		-- next level
		if this.y<-4 and level_index()<30 then next_room() end
		
	end, --<end update loop
	
	draw=function(this)
		-- clamp in screen
		if this.x<-1 or this.x>121 then 
			this.x=clamp(this.x,-1,121)
			this.spd.x=0
		end

		spr(1,this.x,this.y,1,1,this.flip.x,this.flip.y)		
	end
}

player_spawn = {
	tile=1,
	init=function(this)
		this.spr=3
		this.target= {x=this.x,y=this.y}
		this.y=128
		this.spd.y=-4
		this.state=0
		this.delay=0
		this.solids=false
	end,
	update=function(this)
		-- jumping up
		if this.state==0 then
			if this.y < this.target.y+16 then
				this.state=1
				this.delay=3
			end
		-- falling
		elseif this.state==1 then
			this.spd.y = this.spd.y + (0.5)
			if this.spd.y>0 and this.delay>0 then
				this.spd.y=0
				this.delay = this.delay - (1)
			end
			if this.spd.y>0 and this.y > this.target.y then
				this.y=this.target.y
				this.spd = {x=0,y=0}
				this.state=2
				this.delay=5
			end
		-- landing
		elseif this.state==2 then
			this.delay = this.delay - (1)
			this.spr=6
			if this.delay<0 then
				destroy_object(this)
				init_object(player,this.x,this.y)
			end
		end
	end,
}
add(types,player_spawn)

-- object functions --
-----------------------

function init_object(type,x,y)
	local obj = {}
	obj.type = type
	obj.collideable=true
	obj.solids=true

	obj.spr = type.tile
	obj.flip = {x=false,y=false}

	obj.x = x
	obj.y = y
	obj.hitbox = { x=0,y=0,w=8,h=8 }

	obj.spd = {x=0,y=0}
	obj.rem = {x=0,y=0}

	obj.is_solid=function(ox,oy)
		return solid_at(obj.x+obj.hitbox.x+ox,obj.y+obj.hitbox.y+oy,obj.hitbox.w,obj.hitbox.h)
		 or obj.check(fall_floor,ox,oy)
		 or obj.check(fake_wall,ox,oy)
	end
	
	obj.collide=function(type,ox,oy)
		local other
		for i=1,count(objects) do
			other=objects[i]
			if other ~=nil and other.type == type and other ~= obj and other.collideable and
				other.x+other.hitbox.x+other.hitbox.w > obj.x+obj.hitbox.x+ox and 
				other.y+other.hitbox.y+other.hitbox.h > obj.y+obj.hitbox.y+oy and
				other.x+other.hitbox.x < obj.x+obj.hitbox.x+obj.hitbox.w+ox and 
				other.y+other.hitbox.y < obj.y+obj.hitbox.y+obj.hitbox.h+oy then
				return other
			end
		end
		return nil
	end
	
	obj.check=function(type,ox,oy)
		return obj.collide(type,ox,oy) ~=nil
	end
	
	obj.move=function(ox,oy)
		local amount
		-- [x] get move amount
 	obj.rem.x = obj.rem.x + (ox)
		amount = flr(obj.rem.x + 0.5)
		obj.rem.x = obj.rem.x - (amount)
		obj.move_x(amount,0)
		
		-- [y] get move amount
		obj.rem.y = obj.rem.y + (oy)
		amount = flr(obj.rem.y + 0.5)
		obj.rem.y = obj.rem.y - (amount)
		obj.move_y(amount)
	end
	
	obj.move_x=function(amount,start)
		if obj.solids then
			local step = sign(amount)
			for i=start,abs(amount) do
				if not obj.is_solid(step,0) then
					obj.x = obj.x + (step)
				else
					obj.spd.x = 0
					obj.rem.x = 0
					break
				end
			end
		else
			obj.x = obj.x + (amount)
		end
	end
	
	obj.move_y=function(amount)
		if obj.solids then
			local step = sign(amount)
			for i=0,abs(amount) do
	 		if not obj.is_solid(0,step) then
					obj.y = obj.y + (step)
				else
					obj.spd.y = 0
					obj.rem.y = 0
					break
				end
			end
		else
			obj.y = obj.y + (amount)
		end
	end

	add(objects,obj)
	if obj.type.init~=nil then
		obj.type.init(obj)
	end
	return obj
end

function destroy_object(obj)
	del(objects,obj)
end

function kill_player(obj)
	deaths = deaths + (1)
	destroy_object(obj)
	restart_room()
end

-- room functions --
--------------------

function restart_room()
	will_restart=true
	delay_restart=15
end

function next_room()
	if room.x==7 then
		load_room(0,room.y+1)
	else
		load_room(room.x+1,room.y)
	end
end

function load_room(x,y)
	--remove existing objects
	foreach(objects,destroy_object)

	--current room
	room.x = x
	room.y = y

	-- entities
	for tx=0,15 do
		for ty=0,15 do
			local tile = mget(room.x*16+tx,room.y*16+ty);
			foreach(types, 
			function(type) 
				if type.tile == tile then
					init_object(type,tx*8,ty*8) 
				end 
			end)
		end
	end
end

-- update function --
-----------------------

function _update()
	-- cancel if freeze
	if freeze>0 then freeze = freeze - 1 return end

	-- restart (soon)
	if will_restart and delay_restart>0 then
		delay_restart = delay_restart - (1)
		if delay_restart<=0 then
			will_restart=false
			load_room(room.x,room.y)
		end
	end

	-- update each object
	foreach(objects,function(obj)
		if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
			obj.move(obj.spd.x,obj.spd.y)
		end
		if obj.type.update~=nil then
			obj.type.update(obj) 
		end
	end)
end

-- drawing functions --
-----------------------
function _draw()
	if freeze>0 then return end
	
	-- reset all palette values
	pal()
	
	-- clear screen
	local bg_col = 0
	if flash_bg then
		bg_col = frames/5
	elseif new_bg~=nil then
		bg_col=2
	end
	rectfill(0,0,128,128,bg_col)

	-- draw bg terrain
	map(room.x * 16,room.y * 16,0,0,16,16,4)

	-- draw terrain
	map(room.x*16,room.y * 16,0,0,16,16,2)
	
	-- draw objects
	foreach(objects, function(o)
		draw_object(o)
	end)
	
	-- draw fg terrain
	map(room.x * 16,room.y * 16,0,0,16,16,8)
end

function draw_object(obj)
	if obj.type.draw ~=nil then
		obj.type.draw(obj)
	end
end

-- helper functions --
----------------------

function clamp(val,a,b)
	return max(a, min(b, val))
end

function appr(val,target,amount)
 return val > target 
 	and max(val - amount, target) 
 	or min(val + amount, target)
end

function sign(v)
	return v>0 and 1 or
								v<0 and -1 or 0
end

function solid_at(x,y,w,h)
 return tile_flag_at(x,y,w,h,0)
end

function tile_flag_at(x,y,w,h,flag)
 for i=max(0,flr(x/8)),min(15,(x+w-1)/8) do
 	for j=max(0,flr(y/8)),min(15,(y+h-1)/8) do
 		if fget(tile_at(i,j),flag) then
 			return true
 		end
 	end
 end
	return false
end

function tile_at(x,y)
 return mget(room.x * 16 + x, room.y * 16 + y)
end

function spikes_at(x,y,w,h,xspd,yspd)
 for i=max(0,flr(x/8)),min(15,(x+w-1)/8) do
 	for j=max(0,flr(y/8)),min(15,(y+h-1)/8) do
 	 local tile=tile_at(i,j)
 	 if tile==17 and ((y+h-1)%8>=6 or y+h==j*8+8) and yspd>=0 then
 	  return true
 	 elseif tile==27 and y%8<=2 and yspd<=0 then
 	  return true
 		elseif tile==43 and x%8<=2 and xspd<=0 then
 		 return true
 		elseif tile==59 and ((x+w-1)%8>=6 or x+w==i*8+8) and xspd>=0 then
 		 return true
 		end
 	end
 end
	return false
end