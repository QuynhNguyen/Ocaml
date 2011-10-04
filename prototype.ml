let test n =
	if n < 10 then
		0
	else begin
		let rec persistenceHelper n = begin
			let rec addDigits n =
				if n <= 0 then
					0
				else
					((n mod 10) + addDigits (n / 10)) in
				
				let x = addDigits n in
					if x > 9 then
						1 + persistenceHelper x
					else
						1 
		end in persistenceHelper n;
	end